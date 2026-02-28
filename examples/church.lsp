;;; ==========================================================================
;;; church.lsp — Church numerals in Hashlisp
;;; ==========================================================================
;;; Pure lambda calculus encoded as Hashlisp closures.
;;; Shows that the language is expressive enough for abstract encodings.

(display "=== Church Numerals ===") (newline) (newline)

;; Church numeral constructors
(define zero  (lambda (f) (lambda (x) x)))
(define one   (lambda (f) (lambda (x) (f x))))
(define two   (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

;; Successor
(define (succ n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; Addition
(define (church-add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;; Multiplication
(define (church-mul m n)
  (lambda (f) (n (m f))))

;; Convert Church numeral to integer
(define (church->int n)
  ((n (lambda (x) (+ x 1))) 0))

(display "zero  = ") (display (church->int zero)) (newline)
(display "one   = ") (display (church->int one)) (newline)
(display "two   = ") (display (church->int two)) (newline)
(display "three = ") (display (church->int three)) (newline)

(define four (succ three))
(define five (church-add two three))
(define six  (church-mul two three))
(define ten  (church-mul two five))

(display "four (succ three)    = ") (display (church->int four)) (newline)
(display "five (2 + 3)         = ") (display (church->int five)) (newline)
(display "six  (2 * 3)         = ") (display (church->int six)) (newline)
(display "ten  (2 * 5)         = ") (display (church->int ten)) (newline)
(display "hundred (10 * 10)    = ") (display (church->int (church-mul ten ten))) (newline)
(newline)

;; Church booleans
(define church-true  (lambda (t) (lambda (f) t)))
(define church-false (lambda (t) (lambda (f) f)))
(define (church-if c t f) ((c t) f))
(define (church-and a b) ((a b) church-false))
(define (church-or  a b) ((a church-true) b))
(define (church-not a) ((a church-false) church-true))

(define (church-bool->string b)
  (church-if b "true" "false"))

(display "church-true  = ") (display (church-bool->string church-true)) (newline)
(display "church-false = ") (display (church-bool->string church-false)) (newline)
(display "NOT true     = ") (display (church-bool->string (church-not church-true))) (newline)
(display "true AND false = ") (display (church-bool->string (church-and church-true church-false))) (newline)
(display "true OR false  = ") (display (church-bool->string (church-or church-true church-false))) (newline)
