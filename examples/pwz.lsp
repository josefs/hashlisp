;;; ==========================================================================
;;; pwz.lsp — Parsing with Derivatives Library (Hash-Consed)
;;; ==========================================================================
;;; Brzozowski derivative-based parsing where hash-consing provides
;;; automatic structural sharing. The key simplifications:
;;;
;;;   - Identical grammar nodes after derivation are eq?-identical
;;;     (free from the runtime's hash-consed heap).
;;;   - Smart constructors use eq? to simplify: alt(g,g) → g.
;;;   - Parse forests share structure via hash-consing.
;;;   - define-memo can cache derivative computations.
;;;
;;; Grammar representation (tagged lists, all hash-consed):
;;;   (eps)         — empty string, produces ()
;;;   (eps v)       — empty string, produces v
;;;   (tok f)       — matches one token where (f token) is truthy
;;;   (seq g1 g2)   — sequence: g1 then g2; produces (v1 . v2)
;;;   (alt g1 g2)   — alternation: g1 or g2
;;;   (red g f)     — reduction: parse with g, apply f to result
;;;   (ref name)    — named reference (for recursive grammars)
;;;   (empty)       — the empty language (matches nothing)
;;;
;;; Limitation: Left-recursive grammars (e.g. E → E + n | n) are NOT
;;; supported.  The derivative of a left-recursive ref creates a
;;; circular equation; we break the cycle by returning (empty), which
;;; discards the recursive branch.  Use right-recursive grammars
;;; instead (e.g. E → n + E | n).
;;;
;;; This library is based on the paper
;;; "Parsing with Zippers" by Pierce Darragh and Michael D. Adams
;;; Their library can handle left recursion. Hashlisp cannot handle 
;;; that, since it would require `setcar!` and `setcdr!` which doesn't
;;; mix with hash-consing.
;;;
;;; Usage:  (load "examples/pwz.lsp")

;;; ── Utilities ────────────────────────────────────────────────────

(define (assq key alist)
  (cond ((null? alist) #f)
        ((eq? key (caar alist)) (car alist))
        (else (assq key (cdr alist)))))

(define (memq x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) lst)
        (else (memq x (cdr lst)))))

(define (flatmap f lst)
  (if (null? lst) '()
      (append (f (car lst)) (flatmap f (cdr lst)))))

;;; ── Grammar Constructors ─────────────────────────────────────────

(define *eps-node* (list 'eps))
(define (eps)     *eps-node*)
(define (eps* v)  (list 'eps v))
(define (tok f)   (list 'tok f))
(define (seq a b) (list 'seq a b))
(define (alt a b) (list 'alt a b))
(define (red g f) (list 'red g f))
(define (ref name) (list 'ref name))

(define *empty* (list 'empty))
(define (empty) *empty*)
(define (empty? g) (eq? g *empty*))

(define (grammar-tag g) (car g))
(define (grammar-arg1 g) (cadr g))
(define (grammar-arg2 g) (caddr g))

;; Match a specific token
(define (lit c) (tok (lambda (x) (eq? x c))))

;; Sequence of literals
(define (lits chars)
  (if (null? (cdr chars))
      (lit (car chars))
      (seq (lit (car chars)) (lits (cdr chars)))))

;; Alternation over a list
(define (alts gs)
  (if (null? (cdr gs))
      (car gs)
      (alt (car gs) (alts (cdr gs)))))

;;; ── Grammar Table ────────────────────────────────────────────────

(define *grammar-table* '())

(define (define-grammar! name g)
  (set! *grammar-table* (cons (cons name g) *grammar-table*)))

(define (lookup-grammar name)
  (let ((entry (assq name *grammar-table*)))
    (if entry (cdr entry)
        (error "undefined grammar"))))

;;; ── Nullability ──────────────────────────────────────────────────
;;; Returns #f or a list of parse results.
;;; Cycle detection only tracks ref names (cycles only through refs).

(define *nullable-ref-seen* '())

(define (nullable? g)
  (set! *nullable-ref-seen* '())
  (nullable-walk g))

(define (nullable-walk g)
  (cond
    ((empty? g) #f)
    (else
     (let ((tag (grammar-tag g)))
       (cond
         ((eq? tag 'eps)
          (if (null? (cdr g)) (list '()) (list (cadr g))))
         ((eq? tag 'tok) #f)
         ((eq? tag 'seq)
          (let ((n1 (nullable-walk (grammar-arg1 g)))
                (n2 (nullable-walk (grammar-arg2 g))))
            (if (and n1 n2)
                (flatmap (lambda (v1)
                  (map (lambda (v2) (cons v1 v2)) n2))
                  n1)
                #f)))
         ((eq? tag 'alt)
          (let ((n1 (nullable-walk (grammar-arg1 g)))
                (n2 (nullable-walk (grammar-arg2 g))))
            (cond ((and n1 n2) (append n1 n2))
                  (n1 n1) (n2 n2) (else #f))))
         ((eq? tag 'red)
          (let ((n (nullable-walk (grammar-arg1 g))))
            (if n (map (grammar-arg2 g) n) #f)))
         ((eq? tag 'ref)
          (let ((name (grammar-arg1 g)))
            (if (memq name *nullable-ref-seen*)
                #f
                (begin
                  (set! *nullable-ref-seen*
                        (cons name *nullable-ref-seen*))
                  (nullable-walk (lookup-grammar name))))))
         ((eq? tag 'empty) #f)
         (else #f))))))

;;; ── Smart Constructors ───────────────────────────────────────────
;;; Simplify during construction. Hash-consing means eq? comparisons
;;; are free — alt(g, g) → g is just a pointer check.

(define (mk-seq a b)
  (cond ((empty? a) (empty))
        ((empty? b) (empty))
        (else (seq a b))))

(define (mk-alt a b)
  (cond ((empty? a) b)
        ((empty? b) a)
        ((eq? a b) a)         ;; free with hash-consing!
        (else (alt a b))))

(define (mk-red g f)
  (if (empty? g) (empty) (red g f)))

;;; ── Derivative ───────────────────────────────────────────────────
;;; d/dt(g) = grammar matching the rest after consuming token t.
;;;
;;; For (ref name), we break recursion by caching the first visit
;;; as (empty) and computing the derivative of the body. A single
;;; derivation step can only enter each ref once.

(define *deriv-ref-cache* '())

(define (derive g token)
  (set! *deriv-ref-cache* '())
  (derive-walk g token))

(define (derive-walk g token)
  (cond
    ((empty? g) (empty))
    ((eq? (grammar-tag g) 'eps) (empty))

    ((eq? (grammar-tag g) 'tok)
     (if ((grammar-arg1 g) token) (eps* token) (empty)))

    ((eq? (grammar-tag g) 'seq)
     (let* ((g1 (grammar-arg1 g))
            (g2 (grammar-arg2 g))
            (d1 (derive-walk g1 token))
            (left (mk-seq d1 g2))
            (n1 (nullable? g1)))
       (if n1
           (let ((d2 (derive-walk g2 token)))
             ;; For each nullable value v of g1:
             ;; add alt branch:  seq(eps*(v), d2)
             (fold (lambda (v acc)
                     (mk-alt (mk-seq (eps* v) d2) acc))
                   left n1))
           left)))

    ((eq? (grammar-tag g) 'alt)
     (mk-alt (derive-walk (grammar-arg1 g) token)
             (derive-walk (grammar-arg2 g) token)))

    ((eq? (grammar-tag g) 'red)
     (mk-red (derive-walk (grammar-arg1 g) token)
             (grammar-arg2 g)))

    ((eq? (grammar-tag g) 'ref)
     (let* ((name (grammar-arg1 g))
            (cached (assq name *deriv-ref-cache*)))
       (if cached
           ;; Already in progress — return empty to break cycle
           (empty)
           (begin
             ;; Mark as in-progress
             (set! *deriv-ref-cache*
                   (cons (cons name #t) *deriv-ref-cache*))
             (derive-walk (lookup-grammar name) token)))))

    ((eq? (grammar-tag g) 'empty) (empty))
    (else (empty))))

;;; ── Parse ────────────────────────────────────────────────────────

(define (parse grammar tokens)
  (let ((g (derive-all grammar tokens)))
    (if g (nullable? g) #f)))

(define (derive-all grammar tokens)
  (if (null? tokens)
      grammar
      (let ((g (derive grammar (car tokens))))
        (if (empty? g) #f
            (derive-all g (cdr tokens))))))

;;; ── Parse with trace ─────────────────────────────────────────────

(define (parse-trace grammar tokens)
  (display "    grammar: ") (write grammar) (newline)
  (let loop ((g grammar) (toks tokens))
    (if (null? toks)
        (begin
          (display "    nullable? → ")
          (display (nullable? g)) (newline))
        (let ((d (derive g (car toks))))
          (display "    d/d") (display (car toks)) (display " → ")
          (if (empty? d)
              (begin (display "∅") (newline)
                     (display "    result: #f") (newline))
              (begin (write d) (newline)
                     (loop d (cdr toks))))))))

