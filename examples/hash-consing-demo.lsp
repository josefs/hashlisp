;;; ==========================================================================
;;; hash-consing-demo.lsp — Demonstrates the core hash-consing property
;;; ==========================================================================
;;; In Hashlisp, identical structures share the same heap slot.
;;; Two cons cells with the same car and cdr have the SAME hash.

(display "=== Hash-Consing Demo ===") (newline) (newline)

;; Two independently constructed identical lists
(define a (cons 1 (cons 2 (cons 3 '()))))
(define b (cons 1 (cons 2 (cons 3 '()))))

(display "a = ") (display a) (newline)
(display "b = ") (display b) (newline)
(display "a and b built separately, but:") (newline)
(display "  (eq? a b) = ") (display (eq? a b)) (newline)
(display "  hash(a) = ") (display (hash-of a)) (newline)
(display "  hash(b) = ") (display (hash-of b)) (newline)
(newline)

;; Even deeply nested structures are hash-consed
(define tree1 (list (list 1 2) (list 3 4)))
(define tree2 (list (list 1 2) (list 3 4)))
(display "tree1 = ") (display tree1) (newline)
(display "tree2 = ") (display tree2) (newline)
(display "  (eq? tree1 tree2) = ") (display (eq? tree1 tree2)) (newline)
(display "  Same hash? ") (display (= (hash-of tree1) (hash-of tree2))) (newline)
(newline)

;; Subtrees are shared too!
(define sub1 (car tree1))
(define sub2 (car tree2))
(display "Subtree (1 2):") (newline)
(display "  (eq? (car tree1) (car tree2)) = ") (display (eq? sub1 sub2)) (newline)
(newline)

;; Strings are hash-consed too
(define s1 "hello world")
(define s2 "hello world")
(display "s1 = ") (write s1) (newline)
(display "s2 = ") (write s2) (newline)
(display "  (eq? s1 s2) = ") (display (eq? s1 s2)) (newline)
(newline)

;; Heap introspection
(display "Heap size: ") (display (heap-size)) (newline)
(display "Running GC... swept: ") (display (gc)) (display " objects") (newline)
(display "Heap size after GC: ") (display (heap-size)) (newline)
