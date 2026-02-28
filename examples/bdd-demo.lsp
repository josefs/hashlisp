;;; ==========================================================================
;;; bdd-demo.lsp — BDD Demo (loads bdd.lsp)
;;; ==========================================================================

(load "examples/bdd.lsp")

(display "=== BDD Demo (powered by Hash-Consing) ===") (newline) (newline)

(define x1 (bdd-var-node 1))
(define x2 (bdd-var-node 2))
(define x3 (bdd-var-node 3))

(display "Basic BDDs:") (newline)
(display "  x1 AND x2:  ") (display (bdd-and x1 x2)) (newline)
(display "  x1 OR  x2:  ") (display (bdd-or  x1 x2)) (newline)
(display "  x1 XOR x2:  ") (display (bdd-xor x1 x2)) (newline)
(display "  NOT x1:      ") (display (bdd-not x1))    (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 2: O(1) equivalence checking via eq?
;;; ══════════════════════════════════════════════════════════════════
;;;
;;; Because BDD nodes are hash-consed, two BDDs representing the
;;; same boolean function are always the identical object.
;;; Checking equivalence is just (eq? a b) — pointer equality!

(display "Equivalence checking (all O(1) via eq?):") (newline)

;; De Morgan's law: NOT(A AND B) == (NOT A) OR (NOT B)
(define dm-lhs (bdd-not (bdd-and x1 x2)))
(define dm-rhs (bdd-or (bdd-not x1) (bdd-not x2)))
(display "  De Morgan:      NOT(x1 AND x2)  eq?  (NOT x1) OR (NOT x2)  : ")
(display (eq? dm-lhs dm-rhs)) (newline)

;; Commutativity: A XOR B == B XOR A
(display "  Commutativity:  x1 XOR x2       eq?  x2 XOR x1             : ")
(display (eq? (bdd-xor x1 x2) (bdd-xor x2 x1))) (newline)

;; Distributivity: A AND (B OR C) == (A AND B) OR (A AND C)
(define dist-lhs (bdd-and x1 (bdd-or x2 x3)))
(define dist-rhs (bdd-or (bdd-and x1 x2) (bdd-and x1 x3)))
(display "  Distributivity: x1 AND (x2 OR x3) eq? (x1&x2) OR (x1&x3) : ")
(display (eq? dist-lhs dist-rhs)) (newline)

;; XOR associativity: (A XOR B) XOR C == A XOR (B XOR C)
(define xa-lhs (bdd-xor (bdd-xor x1 x2) x3))
(define xa-rhs (bdd-xor x1 (bdd-xor x2 x3)))
(display "  Associativity:  (x1 XOR x2) XOR x3 eq? x1 XOR (x2 XOR x3): ")
(display (eq? xa-lhs xa-rhs)) (newline)

(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 3: The XOR chain — exponential compression!
;;; ══════════════════════════════════════════════════════════════════
;;;
;;; The XOR of n variables:  x1 XOR x2 XOR ... XOR xn
;;;
;;; Without sharing (as a tree):   2^(n+1) - 1 nodes  [EXPONENTIAL]
;;; With hash-consing (as a DAG):  2n - 1 nodes        [LINEAR]
;;;
;;; The hash-consed heap gives us this compression for free —
;;; no explicit sharing code needed!

(define (xor-chain n)
  (let loop ((i 2) (acc (bdd-var-node 1)))
    (if (> i n) acc
        (loop (+ i 1) (bdd-xor acc (bdd-var-node i))))))

(display "XOR chain:  x1 XOR x2 XOR ... XOR xn") (newline)
(display "  Exponential as a tree, linear as a hash-consed DAG:") (newline)
(newline)
(display "   n    DAG nodes    Tree nodes (unshared)    Compression") (newline)
(display "  ---   ---------    --------------------    -----------") (newline)

(define (show-xor-stats n)
  (let* ((bdd (xor-chain n))
         (dag (bdd-dag-size bdd))
         (tree (- (expt 2 (+ n 1)) 1))
         (ratio (floor (/ tree dag))))
    (display "   ")  (display n)
    (display "        ")  (display dag)
    (display "              ")  (display tree)
    (display "              ")  (display ratio)
    (display "x") (newline)))

(show-xor-stats 2)
(show-xor-stats 4)
(show-xor-stats 6)
(show-xor-stats 8)
(show-xor-stats 10)
(show-xor-stats 12)
(show-xor-stats 14)
(newline)

;; The same XOR chain built twice independently → same object!
(define xor12a (xor-chain 12))
(define xor12b (xor-chain 12))
(display "  Two independently built XOR-12 BDDs:") (newline)
(display "    (eq? xor12a xor12b) = ") (display (eq? xor12a xor12b)) (newline)
(display "    Hash-consing made them the exact same heap object!") (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 4: Satisfiability counting
;;; ══════════════════════════════════════════════════════════════════

(display "Satisfiability counting:") (newline)

(define xor5 (xor-chain 5))
(display "  XOR of 5 vars:   ")
(display (bdd-sat-count xor5 5)) (display "/") (display (expt 2 5))
(display " satisfying assignments") (newline)

(define and3 (bdd-and (bdd-and x1 x2) x3))
(display "  x1 AND x2 AND x3: ")
(display (bdd-sat-count and3 3)) (display "/") (display (expt 2 3))
(display " satisfying assignments") (newline)

(define or3 (bdd-or (bdd-or x1 x2) x3))
(display "  x1 OR x2 OR x3:   ")
(display (bdd-sat-count or3 3)) (display "/") (display (expt 2 3))
(display " satisfying assignments") (newline)

;; Majority function: at least 2 of 3 variables are true
(define maj3 (bdd-or (bdd-and x1 x2)
                     (bdd-or (bdd-and x1 x3) (bdd-and x2 x3))))
(display "  MAJORITY(x1,x2,x3): ")
(display (bdd-sat-count maj3 3)) (display "/") (display (expt 2 3))
(display " satisfying assignments") (newline)

(newline)

;; Verify XOR BDD by evaluation
(display "Verification (evaluate XOR-3 on all inputs):") (newline)
(define xor3 (xor-chain 3))
(define (show-eval a b c)
  (let* ((env (list (cons 1 a) (cons 2 b) (cons 3 c)))
         (result (bdd-eval xor3 env)))
    (display "  x1=") (display a)
    (display " x2=") (display b)
    (display " x3=") (display c)
    (display "  =>  ") (display result) (newline)))

(show-eval #f #f #f)
(show-eval #f #f #t)
(show-eval #f #t #f)
(show-eval #f #t #t)
(show-eval #t #f #f)
(show-eval #t #f #t)
(show-eval #t #t #f)
(show-eval #t #t #t)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 5: Heap statistics
;;; ══════════════════════════════════════════════════════════════════

(display "Heap size: ") (display (heap-size)) (newline)
(gc)
(display "Heap size after GC: ") (display (heap-size)) (newline)
(display "  All those BDDs -- including XOR chains that would be") (newline)
(display "  65,535 tree nodes -- fit in a tiny hash-consed heap!") (newline)
