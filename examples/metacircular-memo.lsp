;;; ==========================================================================
;;; metacircular-memo.lsp — Metacircular evaluator with memoized eval
;;; ==========================================================================
;;; Demonstrates the effect of define-memo on the eval function.
;;; Since ASTs and environments are hash-consed, identical (expr, env)
;;; pairs hit the memo cache — turning exponential recursion linear.

(display "=== Metacircular Evaluator: define-memo edition ===") (newline) (newline)

;; Association list lookup
(define (assoc-env key env)
  (cond
    ((null? env) #f)
    ((eq? (caar env) key) (cdar env))
    (else (assoc-env key (cdr env)))))

;; Extend environment with bindings
(define (extend-env params vals env)
  (if (null? params) env
      (cons (cons (car params) (car vals))
            (extend-env (cdr params) (cdr vals) env))))

;; The evaluator — now with define-memo!
;; The memo key is (list expr env), both hash-consed.
(define-memo (my-eval expr env)
  (cond
    ((number? expr) expr)
    ((symbol? expr)
     (let ((val (assoc-env expr env)))
       (if val val
           (error "unbound variable"))))
    ((eq? (car expr) 'quote)
     (cadr expr))
    ((eq? (car expr) 'if)
     (if (my-eval (cadr expr) env)
         (my-eval (caddr expr) env)
         (my-eval (car (cdr (cdr (cdr expr)))) env)))
    ((eq? (car expr) 'lambda)
     (list 'closure (cadr expr) (caddr expr) env))
    ((eq? (car expr) 'define)
     (let ((val (my-eval (caddr expr) env)))
       (cons (cons (cadr expr) val) env)))
    (else
     (let ((fn (my-eval (car expr) env))
           (args (map (lambda (a) (my-eval a env)) (cdr expr))))
       (cond
         ((eq? fn 'prim-add) (+ (car args) (cadr args)))
         ((eq? fn 'prim-sub) (- (car args) (cadr args)))
         ((eq? fn 'prim-mul) (* (car args) (cadr args)))
         ((eq? fn 'prim-lt) (< (car args) (cadr args)))
         ((and (pair? fn) (eq? (car fn) 'closure))
          (let ((params (cadr fn))
                (body   (caddr fn))
                (cenv   (car (cdr (cdr (cdr fn))))))
            (my-eval body (extend-env params args cenv))))
         (else (error "not a function")))))))

;; Initial environment
(define init-env
  (list (cons '+ 'prim-add)
        (cons '- 'prim-sub)
        (cons '* 'prim-mul)
        (cons '< 'prim-lt)))

;;; ── Basic tests ──────────────────────────────────────────────────

(display "3 + 4 = ")
(display (my-eval '(+ 3 4) init-env))
(newline)

(display "((lambda (x) (* x x)) 5) = ")
(display (my-eval '((lambda (x) (* x x)) 5) init-env))
(newline)

(display "fact(7) via self-passing = ")
(display
  (my-eval
    '((lambda (fact)
        (fact fact 7))
      (lambda (self n)
        (if (< n 2) 1
            (* n (self self (- n 1))))))
    init-env))
(newline)

;;; ── The payoff: exponential → linear fibonacci ───────────────────

(display "fib(10) = ")
(display
  (my-eval
    '((lambda (fib)
        (fib fib 10))
      (lambda (self n)
        (if (< n 2) n
            (+ (self self (- n 1))
               (self self (- n 2))))))
    init-env))
(newline)

;; This would be impossibly slow without memoization!
(display "fib(30) = ")
(display
  (my-eval
    '((lambda (fib)
        (fib fib 30))
      (lambda (self n)
        (if (< n 2) n
            (+ (self self (- n 1))
               (self self (- n 2))))))
    init-env))
(newline)

(newline)
(display "Without define-memo, fib(30) would take ~2^30 eval calls.") (newline)
(display "With define-memo, each (expr, env) pair is computed once.") (newline)
(display "Hash-consing ensures identical environments are eq?,") (newline)
(display "so the memo key matches across redundant recursive branches.") (newline)
(newline)
(display "Heap size: ") (display (heap-size)) (newline)
(gc)
(display "After GC: ") (display (heap-size)) (newline)
