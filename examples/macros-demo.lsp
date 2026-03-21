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

;; 'my-or' macro using gensym for hygiene (avoids double evaluation)
(define-macro (my-or . exprs)
  (if (null? exprs) #f
      (let ((tmp (gensym)))
        `(let ((,tmp ,(car exprs)))
           (if ,tmp ,tmp (my-or ,@(cdr exprs)))))))

(display "my-or: ") (display (my-or #f #f 42 99)) (newline)
(display "my-or all false: ") (display (my-or #f #f #f)) (newline)

(newline)

;; 'do-times' macro — loop n times with a counter variable
(define-macro (do-times var n . body)
  `(let loop ((,var 0))
     (when (< ,var ,n)
       ,@body
       (loop (+ ,var 1)))))

(display "do-times: ")
(do-times i 5
  (display i) (display " "))
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
