;;; ==========================================================================
;;; bdd.lsp — Binary Decision Diagram Library (Hash-Consed)
;;; ==========================================================================
;;;
;;; A BDD (Binary Decision Diagram) is a compact canonical form for
;;; boolean functions, stored as a directed acyclic graph (DAG).
;;;
;;; Traditional BDD libraries need TWO hash tables:
;;;   1. A "unique table" — maps (var, low, high) -> existing node
;;;   2. A "computed table" — caches results of apply operations
;;;
;;; With Hashlisp's hash-consed heap, the unique table is FREE!
;;; When we build a node (list var low high), the hash-consing
;;; automatically returns the existing node if one with the same
;;; structure already exists.  This means:
;;;   - Identical sub-BDDs are always pointer-equal: (eq? a b) is O(1)
;;;   - Equivalence checking between boolean functions is O(1)
;;;   - The BDD is stored as a minimal DAG — never an exponential tree
;;;
;;; Usage:  (load "examples/bdd.lsp")

;;; ── BDD Representation ───────────────────────────────────────────
;;;
;;;   Terminal:  #t or #f            (NaN-boxed immediates, no heap)
;;;   Node:     (var . (low . high)) (hash-consed balanced tree)
;;;              var  = variable index (1, 2, 3, ...)
;;;              low  = subtree when var = #f
;;;              high = subtree when var = #t
;;;
;;;   Using (var . (low . high)) instead of (var low high) gives us:
;;;     - 2 cons cells instead of 3 (no nil terminator)
;;;     - Sub-pair (low . high) = the "decision pair" is hash-consed,
;;;       so BDD nodes sharing the same branch pair get free sharing

;; Smart constructor — applies the "no redundant test" reduction rule.
;; Because cons pairs are hash-consed, this also subsumes the unique table!
(define (bdd-mk var low high)
  (if (eq? low high) low              ; reduction: skip redundant test
      (cons var (cons low high))))     ; hash-consed automatically!

(define (bdd-leaf? b) (boolean? b))
(define (bdd-var b) (car b))
(define (bdd-low b) (cadr b))
(define (bdd-high b) (cddr b))

;; BDD for a single variable x_i
(define (bdd-var-node i) (bdd-mk i #f #t))

;;; ── Boolean Operations via Shannon Expansion ─────────────────────
;;;
;;; Hash-consing gives us the unique table for free.  For the
;;; "computed table" we just use define-memo — because all BDD
;;; nodes are hash-consed, (list op f g) is the same key whenever
;;; the inputs are structurally equal.
;;;
;;; We pre-create the operator closures so each one is a single
;;; object, enabling memoization to work across calls too.

(define-memo (bdd-apply op f g)
  (if (and (bdd-leaf? f) (bdd-leaf? g))
      (op f g)
      (cond
        ((bdd-leaf? f)
         (bdd-mk (bdd-var g)
                 (bdd-apply op f (bdd-low g))
                 (bdd-apply op f (bdd-high g))))
        ((bdd-leaf? g)
         (bdd-mk (bdd-var f)
                 (bdd-apply op (bdd-low f) g)
                 (bdd-apply op (bdd-high f) g)))
        ((= (bdd-var f) (bdd-var g))
         (bdd-mk (bdd-var f)
                 (bdd-apply op (bdd-low f) (bdd-low g))
                 (bdd-apply op (bdd-high f) (bdd-high g))))
        ((< (bdd-var f) (bdd-var g))
         (bdd-mk (bdd-var f)
                 (bdd-apply op (bdd-low f) g)
                 (bdd-apply op (bdd-high f) g)))
        (else
         (bdd-mk (bdd-var g)
                 (bdd-apply op f (bdd-low g))
                 (bdd-apply op f (bdd-high g)))))))

;; Pre-created operators — same closure object every time
(define *op-and* (lambda (x y) (and x y)))
(define *op-or*  (lambda (x y) (or x y)))
(define *op-xor* (lambda (x y) (if x (not y) y)))
(define *op-not* (lambda (x y) (not x)))

(define (bdd-and a b)  (bdd-apply *op-and* a b))
(define (bdd-or a b)   (bdd-apply *op-or*  a b))
(define (bdd-xor a b)  (bdd-apply *op-xor* a b))
(define (bdd-not a)    (bdd-apply *op-not* a #t))
(define (bdd-iff a b)  (bdd-not (bdd-xor a b)))

;;; ── Analysis Tools ───────────────────────────────────────────────

;; Is x in lst? (using eq? — O(1) per comparison thanks to hash-consing)
(define (memq-eq? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (else (memq-eq? x (cdr lst)))))

;; Count unique DAG nodes (walks the shared graph, not the tree)
(define (bdd-dag-size b)
  (define (walk b seen)
    (cond
      ((bdd-leaf? b) seen)
      ((memq-eq? b seen) seen)
      (else
        (let* ((seen1 (cons b seen))
               (seen2 (walk (bdd-low b) seen1)))
          (walk (bdd-high b) seen2)))))
  (length (walk b '())))

;; Count satisfying assignments
(define (bdd-sat-count b n-vars)
  (define (count b next-var)
    (cond
      ((eq? b #f) 0)
      ((eq? b #t) (expt 2 (- (+ n-vars 1) next-var)))
      (else
        (let ((skip (- (bdd-var b) next-var)))
          (* (expt 2 skip)
             (+ (count (bdd-low b) (+ (bdd-var b) 1))
                (count (bdd-high b) (+ (bdd-var b) 1))))))))
  (count b 1))

;; Evaluate a BDD given a variable->bool association list
(define (bdd-eval b env)
  (if (bdd-leaf? b) b
      (if (assoc-val (bdd-var b) env)
          (bdd-eval (bdd-high b) env)
          (bdd-eval (bdd-low b) env))))

(define (assoc-val key alist)
  (cond ((null? alist) #f)
        ((= key (caar alist)) (cdar alist))
        (else (assoc-val key (cdr alist)))))

;;; ── Restriction & Quantification ─────────────────────────────────

;; Restrict variable v to value (boolean val) in BDD b.
(define-memo (bdd-restrict b v val)
  (cond
    ((bdd-leaf? b) b)
    ((= (bdd-var b) v) (if val (bdd-high b) (bdd-low b)))
    ((> (bdd-var b) v) b)  ; v not present
    (else (bdd-mk (bdd-var b)
                  (bdd-restrict (bdd-low b) v val)
                  (bdd-restrict (bdd-high b) v val)))))

;; Existential quantification: ∃v. b  =  b[v=0] ∨ b[v=1]
(define (bdd-exists v b)
  (bdd-or (bdd-restrict b v #f) (bdd-restrict b v #t)))

;; Universal quantification: ∀v. b  =  b[v=0] ∧ b[v=1]
(define (bdd-forall v b)
  (bdd-and (bdd-restrict b v #f) (bdd-restrict b v #t)))

;; Quantify out a list of variables
(define (bdd-exists* vars b)
  (fold (lambda (v acc) (bdd-exists v acc)) b vars))

;; Compose: replace variable v with BDD g in BDD b.
;; ite(g, b[v=1], b[v=0])
(define-memo (bdd-compose b v g)
  (cond
    ((bdd-leaf? b) b)
    ((= (bdd-var b) v)
     (bdd-ite g (bdd-high b) (bdd-low b)))
    ((> (bdd-var b) v) b)
    (else (bdd-mk (bdd-var b)
                  (bdd-compose (bdd-low b) v g)
                  (bdd-compose (bdd-high b) v g)))))

;; if-then-else: ite(f, g, h) = (f ∧ g) ∨ (¬f ∧ h)
(define (bdd-ite f g h)
  (bdd-or (bdd-and f g) (bdd-and (bdd-not f) h)))
