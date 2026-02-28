;;; ==========================================================================
;;; macros-demo.lsp — Macro system with quasiquotation demo
;;; ==========================================================================

(display "=== Quasiquote ===") (newline) (newline)

;; Basic quasiquote
(define x 42)
(display `(a b ,x)) (newline)
;; => (a b 42)

;; Nested
(define xs '(1 2 3))
(display `(before ,@xs after)) (newline)
;; => (before 1 2 3 after)

;; Quasiquote with computed parts
(define (make-pair a b) `(,a . ,b))
(display (make-pair 'hello 'world)) (newline)
;; => (hello . world)

(newline)
(display "=== Macros ===") (newline) (newline)

;; Simple 'my-when' macro
(define-macro (my-when test . body)
  `(if ,test (begin ,@body) (void)))

(display "my-when #t: ")
(my-when #t
  (display "yes") (newline))

(display "my-when #f: ")
(my-when #f
  (display "yes") (newline))
(display "(nothing)") (newline)

;; 'swap!' macro using gensym for hygiene
(define-macro (swap! a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (set! ,a ,b)
       (set! ,b ,tmp))))

(define p 10)
(define q 20)
(display "Before swap: p=") (display p) (display " q=") (display q) (newline)
(swap! p q)
(display "After swap:  p=") (display p) (display " q=") (display q) (newline)

(newline)

;; 'while' macro
(define-macro (while test . body)
  `(let loop ()
     (when ,test
       ,@body
       (loop))))

(define i 0)
(display "while loop: ")
(while (< i 5)
  (display i) (display " ")
  (set! i (+ i 1)))
(newline)

;; 'and-let*' -- anaphoric let
(define-macro (and-let* bindings . body)
  (if (null? bindings)
      `(begin ,@body)
      (let ((b (car bindings)))
        `(let ((,(car b) ,(cadr b)))
           (if ,(car b)
               (and-let* ,(cdr bindings) ,@body)
               #f)))))

(display "and-let* success: ")
(display (and-let* ((a 1) (b 2) (c 3))
           (+ a b c)))
(newline)

(display "and-let* short-circuit: ")
(display (and-let* ((a 1) (b #f) (c 3))
           (+ a c)))
(newline)

(newline)

;; macroexpand for debugging
(display "macroexpand (my-when test body):") (newline)
(display (macroexpand '(my-when (> x 0) (display x)))) (newline)

(newline)

;; Hash-consing works through macros!
(define m1 (my-when #t '(1 2 3)))
(define m2 (my-when #t '(1 2 3)))
(display "Hash-consing through macros: (eq? m1 m2) = ")
(display (eq? m1 m2)) (newline)

(display "Heap size: ") (display (heap-size)) (newline)
