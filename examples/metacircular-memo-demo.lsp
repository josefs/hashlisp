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

;;; ── New features work with memoization ───────────────────────────

(display "(let ((x 10) (y 20)) (+ x y)) = ")
(display (my-eval '(let ((x 10) (y 20)) (+ x y)) init-env))
(newline)

(display "(cond (#f 'a) (#t 'b)) = ")
(display (my-eval '(cond (#f (quote a)) (#t (quote b))) init-env))
(newline)

(display "(and 1 2 3) = ")
(display (my-eval '(and 1 2 3) init-env))
(newline)

(display "(or #f 42) = ")
(display (my-eval '(or #f 42) init-env))
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

;;; ── letrec + memoization ─────────────────────────────────────────

(display "fib(25) via letrec = ")
(display
  (my-eval
    '(letrec ((fib (lambda (n)
                     (if (< n 2) n
                         (+ (fib (- n 1)) (fib (- n 2)))))))
       (fib 25))
    init-env))
(newline)

;;; ── Named let (iterative, TCO + memo) ───────────────────────────

(display "sum 1..1000 via named let = ")
(display
  (my-eval
    '(let loop ((i 1000) (acc 0))
       (if (= i 0) acc
           (loop (- i 1) (+ acc i))))
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
