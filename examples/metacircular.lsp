;;; ==========================================================================
;;; metacircular.lsp — A tiny metacircular evaluator running inside Hashlisp
;;; ==========================================================================
;;; An evaluator for a mini-Lisp written in Hashlisp itself!
;;; Supports: numbers, +, -, *, if, lambda, application, define.

(display "=== Metacircular Evaluator ===") (newline) (newline)

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

;; The evaluator
(define (my-eval expr env)
  (cond
    ;; Self-evaluating: numbers
    ((number? expr) expr)
    ;; Variable lookup
    ((symbol? expr)
     (let ((val (assoc-env expr env)))
       (if val val
           (error (string-append "unbound: " (number->string 0))))))
    ;; Quote
    ((eq? (car expr) 'quote)
     (cadr expr))
    ;; If
    ((eq? (car expr) 'if)
     (if (my-eval (cadr expr) env)
         (my-eval (caddr expr) env)
         (my-eval (car (cdr (cdr (cdr expr)))) env)))
    ;; Lambda → closure as (closure params body env)
    ((eq? (car expr) 'lambda)
     (list 'closure (cadr expr) (caddr expr) env))
    ;; Define
    ((eq? (car expr) 'define)
     (let ((val (my-eval (caddr expr) env)))
       (cons (cons (cadr expr) val) env)))
    ;; Application
    (else
     (let ((fn (my-eval (car expr) env))
           (args (map (lambda (a) (my-eval a env)) (cdr expr))))
       (cond
         ;; Built-in +
         ((eq? fn 'prim-add) (+ (car args) (cadr args)))
         ;; Built-in -
         ((eq? fn 'prim-sub) (- (car args) (cadr args)))
         ;; Built-in *
         ((eq? fn 'prim-mul) (* (car args) (cadr args)))
         ;; Built-in <
         ((eq? fn 'prim-lt) (< (car args) (cadr args)))
         ;; Closure
         ((and (pair? fn) (eq? (car fn) 'closure))
          (let ((params (cadr fn))
                (body   (caddr fn))
                (cenv   (car (cdr (cdr (cdr fn))))))
            (my-eval body (extend-env params args cenv))))
         (else (error "not a function")))))))

;; Initial environment with primitives
(define init-env
  (list (cons '+ 'prim-add)
        (cons '- 'prim-sub)
        (cons '* 'prim-mul)
        (cons '< 'prim-lt)))

;; Test it!

;; Simple arithmetic
(display "3 + 4 = ")
(display (my-eval '(+ 3 4) init-env))
(newline)

;; Lambda and application
(display "((lambda (x) (* x x)) 5) = ")
(display (my-eval '((lambda (x) (* x x)) 5) init-env))
(newline)

;; Factorial via self-application (Y-combinator style)
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

;; Fibonacci via self-passing
(display "fib(10) via self-passing = ")
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

(newline)
(display "The metacircular evaluator's closures live on the hash-consed heap!") (newline)
(display "Heap size: ") (display (heap-size)) (newline)
