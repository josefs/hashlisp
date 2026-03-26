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
;; The memo key is (expr, env), both hash-consed.
(define-memo (my-eval expr env)
  (cond
    ;; Self-evaluating: numbers, booleans, strings, void, nil
    ((number? expr)  expr)
    ((boolean? expr) expr)
    ((string? expr)  expr)
    ((null? expr)    expr)
    ((void? expr)    expr)

    ;; Variable lookup
    ((symbol? expr)
     (let ((val (env-lookup expr env)))
       (if val val
           (error (string-append "my-eval: unbound variable")))))

    ;; Special forms (car is a symbol)
    ((pair? expr)
     (let ((head (car expr)))
       (cond
         ((eq? head 'quote)
          (cadr expr))

         ((eq? head 'if)
          (let ((test-val (my-eval (cadr expr) env)))
            (if test-val
                (my-eval (caddr expr) env)
                (if (null? (cdr (cddr expr)))
                    (void)
                    (my-eval (car (cdr (cddr expr))) env)))))

         ((eq? head 'cond)
          (eval-cond (cdr expr) env))

         ((eq? head 'and)
          (eval-and (cdr expr) env))

         ((eq? head 'or)
          (eval-or (cdr expr) env))

         ((eq? head 'when)
          (if (my-eval (cadr expr) env)
              (eval-body (cddr expr) env)
              (void)))

         ((eq? head 'unless)
          (if (not (my-eval (cadr expr) env))
              (eval-body (cddr expr) env)
              (void)))

         ((eq? head 'begin)
          (eval-body (cdr expr) env))

         ((eq? head 'lambda)
          (let ((params (cadr expr))
                (body   (cddr expr)))
            (list 'closure params body env)))

         ((eq? head 'define)
          (eval-define (cdr expr) env))

         ((eq? head 'let)
          (eval-let (cdr expr) env))

         ((eq? head 'let*)
          (eval-let-star (cdr expr) env))

         ((eq? head 'letrec)
          (eval-letrec (cdr expr) env))

         ((eq? head 'quasiquote)
          (eval-quasiquote (cadr expr) env))

         ((eq? head 'apply)
          (let ((fn   (my-eval (cadr expr) env))
                (args (my-eval (caddr expr) env)))
            (apply-fn fn args)))

         (else
          (let ((fn   (my-eval (car expr) env))
                (args (eval-args (cdr expr) env)))
            (apply-fn fn args))))))

    (else (error "my-eval: cannot evaluate expression"))))
