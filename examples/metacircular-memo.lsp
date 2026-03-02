;;; ==========================================================================
;;; metacircular-memo.lsp — Metacircular evaluator with memoized eval
;;; ==========================================================================
;;; Demonstrates the effect of define-memo on the eval function.
;;; Since ASTs and environments are hash-consed, identical (expr, env)
;;; pairs hit the memo cache — turning exponential recursion linear.
;;;
;;; Provides: memoized my-eval (overrides metacircular.lsp's version)
;;; Demo:     metacircular-memo-demo.lsp

(load "examples/metacircular.lsp")

;; Override my-eval with a memoized version.
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
