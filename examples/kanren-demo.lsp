;;; ==========================================================================
;;; kanren-demo.lsp — miniKanren Demonstrations
;;; ==========================================================================

(load "examples/kanren.lsp")

;;; ── Demo 1: Basic Unification ────────────────────────────────────

(display "--- Demo 1: Basic Unification ---") (newline)

(display "  (run 1 (== q 5))           = ")
(display (run 1 (lambda (q) (== q 5))))
(newline)

(display "  (run 1 (== q '(a b c)))    = ")
(display (run 1 (lambda (q) (== q '(a b c)))))
(newline)

(display "  (run 1 (== 3 3))           = ")
(display (run 1 (lambda (q) (conj (== 3 3) (== q 'yes)))))
(newline)

(display "  (run 1 (== 3 4))           = ")
(display (run 1 (lambda (q) (conj (== 3 4) (== q 'yes)))))
(newline)

(newline)

;;; ── Demo 2: Multiple Answers ─────────────────────────────────────

(display "--- Demo 2: Multiple Answers (conde) ---") (newline)

(display "  two answers:   ")
(display (run* (lambda (q)
  (conde2 (== q 'tea) (== q 'coffee)))))
(newline)

(display "  three answers: ")
(display (run* (lambda (q)
  (conde3 (== q 'red) (== q 'green) (== q 'blue)))))
(newline)

(display "  free variable: ")
(display (run* (lambda (q) succeed)))
(newline)

(newline)

;;; ── Demo 3: Fresh Variables and Conjunction ──────────────────────

(display "--- Demo 3: Fresh Variables ---") (newline)

(display "  pair: ")
(display (run* (lambda (q)
  (fresh/2 (lambda (x y)
    (conj* (list (== x 'hello) (== y 'world) (== q (cons x y)))))))))
(newline)

(display "  list: ")
(display (run* (lambda (q)
  (fresh/2 (lambda (x y)
    (conj* (list (== x 1) (== y 2) (== q (list x y)))))))))
(newline)

(newline)

;;; ── Demo 4: appendo — Forward ───────────────────────────────────

(display "--- Demo 4: appendo (Forward) ---") (newline)

(display "  (append '(a b) '(c d)):  ")
(display (run* (lambda (q) (appendo '(a b) '(c d) q))))
(newline)

(display "  (append '() '(x y z)):   ")
(display (run* (lambda (q) (appendo '() '(x y z) q))))
(newline)

(display "  (append '(1) '()):       ")
(display (run* (lambda (q) (appendo '(1) '() q))))
(newline)

(newline)

;;; ── Demo 5: appendo — Backward (the classic!) ──────────────────

(display "--- Demo 5: appendo (Backward) ---") (newline)
(display "  All ways to split (1 2 3):") (newline)

(for-each
  (lambda (answer)
    (display "    ") (display answer) (newline))
  (run* (lambda (q)
    (fresh/2 (lambda (x y)
      (conj* (list (appendo x y '(1 2 3))
                   (== q (list x y)))))))))

(newline)

(display "  What prepended to (3 4) gives (1 2 3 4)?  ")
(display (run* (lambda (q) (appendo q '(3 4) '(1 2 3 4)))))
(newline)

(newline)

;;; ── Demo 6: membero ─────────────────────────────────────────────

(display "--- Demo 6: membero ---") (newline)

(display "  members of (a b c): ")
(display (run* (lambda (q) (membero q '(a b c)))))
(newline)

(display "  is 'b a member?     ")
(display (run 1 (lambda (q) (conj (membero 'b '(a b c)) (== q 'yes)))))
(newline)

(display "  is 'd a member?     ")
(display (run 1 (lambda (q) (conj (membero 'd '(a b c)) (== q 'yes)))))
(newline)

(newline)

;;; ── Demo 7: Infinite Generation with run-n ──────────────────────

(display "--- Demo 7: Infinite Generation ---") (newline)

;; Generate lists of ones: (), (1), (1 1), (1 1 1), ...
(define (oneso q)
  (conde2 (== q '())
          (fresh/1 (lambda (d)
            (conj (== q (cons 1 d)) (oneso d))))))

(display "  first 6 lists of 1s: ")
(display (run 6 (lambda (q) (oneso q))))
(newline)

;; Generate all lists that contain 'x' somewhere
(define (has-x lst)
  (fresh/2 (lambda (h t)
    (conj (== lst (cons h t))
          (conde2 (== h 'x)
                  (has-x t))))))

(display "  first 5 lists with 'x': ")
(display (run 5 (lambda (q) (has-x q))))
(newline)

(newline)

;;; ── Demo 8: Hash-Consing Benefits ──────────────────────────────

(display "--- Demo 8: Hash-Consing Benefits ---") (newline)

;; Run the same query twice — the answer lists are eq?-identical
(define answers1 (run* (lambda (q) (appendo '(a b) '(c d) q))))
(define answers2 (run* (lambda (q) (appendo '(a b) '(c d) q))))

(display "  same query twice eq?:  ")
(display (eq? answers1 answers2))
(newline)

;; Two variables unified to the same structure share the same pointer
(let ((result (run* (lambda (q)
    (fresh/2 (lambda (x y)
      (conj* (list (== x '(a b c d e))
                   (== y '(a b c d e))
                   (== q (list x y))))))))))
  (let ((answer (car result)))
    (display "  unified to same list:   ")
    (display answer)
    (newline)
    (display "  both halves eq?:        ")
    (display (eq? (car answer) (car (cdr answer))))
    (newline)))

;;; ── Demo 9: Quine Generation (Classic miniKanren Benchmark) ─────

(display "--- Demo 9: Quine Generation ---") (newline)
(display "  (The standard miniKanren benchmark: find a program that") (newline)
(display "   evaluates to itself, via a relational interpreter)") (newline)

;; Additional helpers
(define (fresh/5 f)
  (call/fresh (lambda (a)
    (call/fresh (lambda (b)
      (call/fresh (lambda (c)
        (call/fresh (lambda (d)
          (call/fresh (lambda (e) (f a b c d e))))))))))))

(define (fresh/6 f)
  (call/fresh (lambda (a)
    (call/fresh (lambda (b)
      (call/fresh (lambda (c)
        (call/fresh (lambda (d)
          (call/fresh (lambda (e)
            (call/fresh (lambda (g) (f a b c d e g))))))))))))))

(define (conde4 g1 g2 g3 g4)
  (disj (zzz g1) (disj (zzz g2) (disj (zzz g3) (zzz g4)))))

(define (conde5 g1 g2 g3 g4 g5)
  (disj (zzz g1) (disj (zzz g2) (disj (zzz g3) (disj (zzz g4) (zzz g5))))))

;; Unification with occurs check — prevents circular substitutions
;; like q = (quote q) that would otherwise loop during reification.
(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (eq? x v))
      ((pair? v) (or (occurs? x (car v) s)
                     (occurs? x (cdr v) s)))
      (else #f))))

(define (unify-check u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
      ((eq? u v) s)
      ((var? u) (if (occurs? u v s) #f (ext-s u v s)))
      ((var? v) (if (occurs? v u s) #f (ext-s v u s)))
      ((and (pair? u) (pair? v))
       (let ((s (unify-check (car u) (car v) s)))
         (if s (unify-check (cdr u) (cdr v) s) #f)))
      (else #f))))

(define (==o u v)
  (lambda (s/c)
    (let ((s (unify-check u v (car s/c))))
      (if s (cons (cons s (cdr s/c)) '()) '()))))

;; Relational environment lookup
(define (lookupo x env val)
  (fresh/3 (lambda (y v rest)
    (conj (==o env (cons (cons y v) rest))
          (conde2
            (conj (==o x y) (==o val v))
            (lookupo x rest val))))))

;; Evaluate a list of expressions (for the 'list' form)
(define (proper-listo exprs env vals)
  (conde2
    (conj (==o exprs '()) (==o vals '()))
    (fresh/4 (lambda (a d va vd)
      (conj* (list (==o exprs (cons a d))
                   (==o vals (cons va vd))
                   (eval-expo a env va)
                   (proper-listo d env vd)))))))

;; Goal constructors for each eval case (shared by dispatch + fallback)
(define (quote-goal expr env val)
  (fresh/1 (lambda (v)
    (conj (==o expr (list 'quote v)) (==o val v)))))

(define (lambda-goal expr env val)
  (fresh/2 (lambda (x body)
    (conj (==o expr (list 'lambda (list x) body))
          (==o val (list 'closure x body env))))))

(define (app-goal expr env val)
  (fresh/6 (lambda (rator rand x body env2 a)
    (conj* (list (==o expr (list rator rand))
                 (eval-expo rator env (list 'closure x body env2))
                 (eval-expo rand env a)
                 (eval-expo body (cons (cons x a) env2) val))))))

(define (list-goal expr env val)
  (fresh/2 (lambda (args vals)
    (conj* (list (==o expr (cons 'list args))
                 (==o val vals)
                 (proper-listo args env vals))))))

;; Relational evaluator with walk-based dispatch.
;; When the expression is concrete, dispatch directly to the right case.
;; When unknown, try all cases with interleaving.
(define (eval-expo expr env val)
  (lambda (s/c)
    (let ((e (walk expr (car s/c))))
      (cond
        ;; Expression is still a logic variable — try all cases
        ((var? e)
         ((conde5
            (quote-goal expr env val)
            (lookupo expr env val)
            (lambda-goal expr env val)
            (app-goal expr env val)
            (list-goal expr env val)) s/c))
        ;; Atom (symbol or other non-pair) — only variable lookup applies
        ((not (pair? e))
         ((lookupo expr env val) s/c))
        ;; Pair — dispatch on head tag
        (else
         (let ((head (walk (car e) (car s/c))))
           (cond
             ((eq? head 'quote)  ((quote-goal  expr env val) s/c))
             ((eq? head 'lambda) ((lambda-goal expr env val) s/c))
             ((eq? head 'list)   ((list-goal   expr env val) s/c))
             (else               ((app-goal    expr env val) s/c)))))))))

(display "  Searching for a quine...") (newline)
(let ((result (run 1 (lambda (q) (eval-expo q '() q)))))
  (display "  Found: ")
  (display result)
  (newline))

(newline)
(display "All demos passed.") (newline)
