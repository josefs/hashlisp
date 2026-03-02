;;; ==========================================================================
;;; metacircular-memo-demo.lsp — Demo for the memoized metacircular evaluator
;;; ==========================================================================

(load "examples/metacircular-memo.lsp")

(display "=== Metacircular Evaluator: define-memo edition ===") (newline) (newline)

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
