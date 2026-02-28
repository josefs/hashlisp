;;; ==========================================================================
;;; streams.lsp — Lazy streams via thunks (closures on the hash-consed heap)
;;; ==========================================================================
;;; Demonstrates closures + higher-order functions.

(display "=== Lazy Streams ===") (newline) (newline)

;; A stream is a pair (head . thunk-for-tail)
;; where thunk-for-tail is a zero-argument closure that returns the next stream.

(define (stream-cons head tail-thunk)
  (cons head tail-thunk))

(define (stream-head s) (car s))
(define (stream-tail s) ((cdr s)))

(define (stream-take n s)
  (if (= n 0) '()
      (cons (stream-head s)
            (stream-take (- n 1) (stream-tail s)))))

(define (stream-map f s)
  (stream-cons (f (stream-head s))
               (lambda () (stream-map f (stream-tail s)))))

(define (stream-filter pred s)
  (if (pred (stream-head s))
      (stream-cons (stream-head s)
                   (lambda () (stream-filter pred (stream-tail s))))
      (stream-filter pred (stream-tail s))))

(define (stream-zip-with f s1 s2)
  (stream-cons (f (stream-head s1) (stream-head s2))
               (lambda () (stream-zip-with f (stream-tail s1) (stream-tail s2)))))

;; Natural numbers
(define (nats-from n)
  (stream-cons n (lambda () (nats-from (+ n 1)))))

(define nats (nats-from 0))

(display "First 15 naturals: ")
(display (stream-take 15 nats))
(newline)

;; Squares
(define squares (stream-map (lambda (x) (* x x)) (nats-from 1)))
(display "First 10 squares:  ")
(display (stream-take 10 squares))
(newline)

;; Fibonacci stream
(define fibs
  (stream-cons 0
    (lambda ()
      (stream-cons 1
        (lambda ()
          (stream-zip-with + fibs (stream-tail fibs)))))))

(display "First 15 fibs:     ")
(display (stream-take 15 fibs))
(newline)

;; Sieve of Eratosthenes
(define (sieve s)
  (let ((p (stream-head s)))
    (stream-cons p
      (lambda ()
        (sieve (stream-filter
                 (lambda (n) (not (= (modulo n p) 0)))
                 (stream-tail s)))))))

(define primes (sieve (nats-from 2)))

(display "First 20 primes:   ")
(display (stream-take 20 primes))
(newline)

;; Twin primes
(define (twin-primes-from s)
  (let ((p (stream-head s))
        (q (stream-head (stream-tail s))))
    (if (= (- q p) 2)
        (stream-cons (list p q)
                     (lambda () (twin-primes-from (stream-tail s))))
        (twin-primes-from (stream-tail s)))))

(display "First 8 twin primes: ")
(display (stream-take 8 (twin-primes-from primes)))
(newline)
(newline)

(display "Heap size: ") (display (heap-size)) (newline)
(display "Streams are built from closures on the hash-consed heap!") (newline)
