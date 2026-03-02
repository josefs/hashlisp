;;; ==========================================================================
;;; metacircular-demo.lsp — Demo for the metacircular evaluator
;;; ==========================================================================

(load "examples/metacircular.lsp")

(display "=== Metacircular Evaluator ===") (newline) (newline)

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
