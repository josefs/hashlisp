;;; ==========================================================================
;;; metacircular-demo.lsp — Demo for the metacircular evaluator
;;; ==========================================================================

(load "examples/metacircular.lsp")

(display "=== Metacircular Evaluator ===") (newline) (newline)

;;; ── Basic arithmetic ────────────────────────────────────────────

(display "3 + 4 = ")
(display (my-eval '(+ 3 4) init-env))
(newline)

(display "(- 10 3) = ")
(display (my-eval '(- 10 3) init-env))
(newline)

(display "(* 6 7) = ")
(display (my-eval '(* 6 7) init-env))
(newline)

(display "(/ 42 6) = ")
(display (my-eval '(/ 42 6) init-env))
(newline)

(display "(modulo 17 5) = ")
(display (my-eval '(modulo 17 5) init-env))
(newline)

;;; ── Booleans and comparisons ────────────────────────────────────

(newline)
(display "--- Booleans & comparisons ---") (newline)

(display "(= 3 3) = ")
(display (my-eval '(= 3 3) init-env))
(newline)

(display "(< 2 5) = ")
(display (my-eval '(< 2 5) init-env))
(newline)

(display "(> 10 3) = ")
(display (my-eval '(> 10 3) init-env))
(newline)

(display "(not #f) = ")
(display (my-eval '(not #f) init-env))
(newline)

;;; ── Lambda and application ──────────────────────────────────────

(newline)
(display "--- Lambda ---") (newline)

(display "((lambda (x) (* x x)) 5) = ")
(display (my-eval '((lambda (x) (* x x)) 5) init-env))
(newline)

;; Multi-body lambda
(display "multi-body lambda (side-effect then result) = ")
(display (my-eval '((lambda (x)
                      (+ 0 0)   ;; first body expression (discarded)
                      (* x x))  ;; second body expression (returned)
                    5)
                  init-env))
(newline)

;; Variadic lambda: (lambda args ...)
(display "(lambda args (length args)) applied to (1 2 3 4 5) = ")
(display (my-eval '((lambda args (length args)) 1 2 3 4 5)
                  init-env))
(newline)

;; Variadic with required params: (lambda (x y . rest) ...)
(display "(lambda (x y . rest) rest) applied to (1 2 3 4) = ")
(display (my-eval '((lambda (x y . rest) rest) 1 2 3 4)
                  init-env))
(newline)

;;; ── quote ───────────────────────────────────────────────────────

(newline)
(display "--- Quote ---") (newline)

(display "(quote (a b c)) = ")
(display (my-eval '(quote (a b c)) init-env))
(newline)

;;; ── if / cond ───────────────────────────────────────────────────

(newline)
(display "--- if / cond ---") (newline)

(display "(if (< 1 2) 'yes 'no) = ")
(display (my-eval '(if (< 1 2) (quote yes) (quote no)) init-env))
(newline)

(display "(cond ((> 3 5) 'a) ((< 3 5) 'b) (else 'c)) = ")
(display (my-eval '(cond ((> 3 5) (quote a))
                         ((< 3 5) (quote b))
                         (else (quote c)))
                  init-env))
(newline)

;;; ── and / or ────────────────────────────────────────────────────

(newline)
(display "--- and / or ---") (newline)

(display "(and 1 2 3) = ")
(display (my-eval '(and 1 2 3) init-env))
(newline)

(display "(and 1 #f 3) = ")
(display (my-eval '(and 1 #f 3) init-env))
(newline)

(display "(or #f #f 42) = ")
(display (my-eval '(or #f #f 42) init-env))
(newline)

;;; ── when / unless ───────────────────────────────────────────────

(newline)
(display "--- when / unless ---") (newline)

(display "(when (< 1 2) 'yes) = ")
(display (my-eval '(when (< 1 2) (quote yes)) init-env))
(newline)

(display "(unless (< 1 2) 'yes) = ")
(display (my-eval '(unless (< 1 2) (quote yes)) init-env))
(newline)

;;; ── begin ───────────────────────────────────────────────────────

(newline)
(display "--- begin ---") (newline)

(display "(begin 1 2 3) = ")
(display (my-eval '(begin 1 2 3) init-env))
(newline)

;;; ── define ──────────────────────────────────────────────────────

(newline)
(display "--- define ---") (newline)

;; define variable then use it
(display "define x=42, x+1 = ")
(let ((env1 (my-eval '(define x 42) init-env)))
  (display (my-eval '(+ x 1) env1)))
(newline)

;; define function shorthand
(display "define (square x), (square 7) = ")
(let ((env1 (my-eval '(define (square x) (* x x)) init-env)))
  (display (my-eval '(square 7) env1)))
(newline)

;;; ── let / let* / letrec / named let ─────────────────────────────

(newline)
(display "--- let / let* / letrec / named let ---") (newline)

(display "(let ((x 1) (y 2)) (+ x y)) = ")
(display (my-eval '(let ((x 1) (y 2)) (+ x y)) init-env))
(newline)

(display "(let* ((x 1) (y (+ x 1))) (+ x y)) = ")
(display (my-eval '(let* ((x 1) (y (+ x 1))) (+ x y)) init-env))
(newline)

(display "(letrec ((f (lambda (n) (if (< n 2) 1 (* n (f (- n 1))))))) (f 7)) = ")
(display (my-eval '(letrec ((f (lambda (n)
                                 (if (< n 2) 1
                                     (* n (f (- n 1)))))))
                    (f 7))
                  init-env))
(newline)

;; Named let: iterative factorial
(display "named-let fact(10) = ")
(display (my-eval '(let loop ((n 10) (acc 1))
                     (if (< n 2) acc
                         (loop (- n 1) (* n acc))))
                  init-env))
(newline)

;;; ── quasiquote ──────────────────────────────────────────────────

(newline)
(display "--- quasiquote ---") (newline)

(display "`(a ,(+ 1 2) b) = ")
(display (my-eval '(quasiquote (a (unquote (+ 1 2)) b)) init-env))
(newline)

(display "`(a ,@(list 1 2 3) b) = ")
(display (my-eval
  '(let ((xs (list 1 2 3)))
     (quasiquote (a (unquote-splicing xs) b)))
  init-env))
(newline)

;;; ── list operations ─────────────────────────────────────────────

(newline)
(display "--- List operations ---") (newline)

(display "(list 1 2 3) = ")
(display (my-eval '(list 1 2 3) init-env))
(newline)

(display "(cons 1 (cons 2 (cons 3 '()))) = ")
(display (my-eval '(cons 1 (cons 2 (cons 3 (quote ())))) init-env))
(newline)

(display "(car (list 1 2 3)) = ")
(display (my-eval '(car (list 1 2 3)) init-env))
(newline)

(display "(cdr (list 1 2 3)) = ")
(display (my-eval '(cdr (list 1 2 3)) init-env))
(newline)

(display "(length (list 1 2 3 4 5)) = ")
(display (my-eval '(length (list 1 2 3 4 5)) init-env))
(newline)

(display "(append (list 1 2) (list 3 4)) = ")
(display (my-eval '(append (list 1 2) (list 3 4)) init-env))
(newline)

(display "(reverse (list 1 2 3)) = ")
(display (my-eval '(reverse (list 1 2 3)) init-env))
(newline)

;;; ── Higher-order: map / filter / fold ───────────────────────────

(newline)
(display "--- Higher-order functions ---") (newline)

(display "(map (lambda (x) (* x x)) '(1 2 3 4)) = ")
(display (my-eval '(map (lambda (x) (* x x)) (list 1 2 3 4)) init-env))
(newline)

(display "(filter (lambda (x) (> x 3)) '(1 5 2 8 3)) = ")
(display (my-eval '(filter (lambda (x) (> x 3)) (list 1 5 2 8 3)) init-env))
(newline)

(display "(fold (lambda (x acc) (+ x acc)) 0 '(1 2 3 4)) = ")
(display (my-eval '(fold (lambda (x acc) (+ x acc)) 0 (list 1 2 3 4)) init-env))
(newline)

;;; ── apply ───────────────────────────────────────────────────────

(newline)
(display "--- apply ---") (newline)

(display "(apply + (list 1 2 3)) = ")
(display (my-eval '(apply + (list 1 2 3)) init-env))
(newline)

;;; ── Factorial via self-passing (classic test) ───────────────────

(newline)
(display "--- Classic recursion ---") (newline)

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

;;; ── Fibonacci via named let (TCO) ──────────────────────────────

(display "fib(10) via named let = ")
(display
  (my-eval
    '(let fib-iter ((n 10) (a 0) (b 1))
       (if (= n 0) a
           (fib-iter (- n 1) b (+ a b))))
    init-env))
(newline)

;;; ── Fibonacci via letrec ────────────────────────────────────────

(display "fib(10) via letrec = ")
(display
  (my-eval
    '(letrec ((fib (lambda (n)
                     (if (< n 2) n
                         (+ (fib (- n 1)) (fib (- n 2)))))))
       (fib 10))
    init-env))
(newline)

;;; ── Tail-call optimization: deep recursion ──────────────────────

(newline)
(display "--- TCO stress test ---") (newline)

(display "sum 1..1000 via named let (TCO) = ")
(display
  (my-eval
    '(let loop ((i 1000) (acc 0))
       (if (= i 0) acc
           (loop (- i 1) (+ acc i))))
    init-env))
(newline)

(newline)
(display "The metacircular evaluator's closures live on the hash-consed heap!") (newline)
(display "Heap size: ") (display (heap-size)) (newline)
