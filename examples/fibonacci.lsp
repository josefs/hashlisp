;;; ==========================================================================
;;; fibonacci.lsp — Classic Fibonacci, multiple approaches
;;; ==========================================================================

(display "=== Fibonacci ===") (newline) (newline)

;; Naive recursive (exponential)
(define (fib-naive n)
  (if (< n 2) n
      (+ (fib-naive (- n 1)) (fib-naive (- n 2)))))

;; Tail-recursive with accumulator (linear, uses TCO)
(define (fib n)
  (define (loop i a b)
    (if (= i n) a
        (loop (+ i 1) b (+ a b))))
  (loop 0 0 1))

;; Using named let
(define (fib-named-let n)
  (let loop ((i 0) (a 0) (b 1))
    (if (= i n) a
        (loop (+ i 1) b (+ a b)))))

(display "fib(0)  = ") (display (fib 0)) (newline)
(display "fib(1)  = ") (display (fib 1)) (newline)
(display "fib(10) = ") (display (fib 10)) (newline)
(display "fib(20) = ") (display (fib 20)) (newline)
(display "fib(30) = ") (display (fib 30)) (newline)
(newline)

;; Generate first 15 fibs as a list
(define (fibs-upto n)
  (let loop ((i 0) (acc '()))
    (if (> i n) (reverse acc)
        (loop (+ i 1) (cons (fib i) acc)))))

(display "First 15 fibs: ") (display (fibs-upto 14)) (newline)
(newline)

;; Demonstrate that identical sub-lists are hash-consed
(define f10a (fibs-upto 10))
(define f10b (fibs-upto 10))
(display "Two independently computed fib lists, (eq? f10a f10b) = ")
(display (eq? f10a f10b)) (newline)
(display "  (This is #t because hash-consing deduplicates identical structures!)")
(newline)
