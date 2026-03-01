;;; ==========================================================================
;;; let-insertion-demo.lsp — Demo of DAG serialization via let-insertion
;;; ==========================================================================
;;; Demonstrates symbolic differentiation with hash-consed sharing and
;;; let-insertion to serialize the DAG into a readable let* form.

(load "examples/let-insertion.lsp")

(display "=== Symbolic Differentiation DSL with Let-Insertion ===") (newline)
(newline)

;;; ══════════════════════════════════════════════════════════════════
;;; Demo
;;; ══════════════════════════════════════════════════════════════════

;; Build x^8 via repeated squaring — minimal DAG, maximal tree blowup
(define e1 (make-mul 'x 'x))
(define e2 (make-mul e1 e1))
(define e3 (make-mul e2 e2))

(display "── Building x^8 via repeated squaring ──") (newline)
(display "  e1 = (* x x)      = x^2") (newline)
(display "  e2 = (* e1 e1)    = x^4") (newline)
(display "  e3 = (* e2 e2)    = x^8") (newline)
(display "  Naive print of e3: ") (write e3) (newline)
(newline)

;; Differentiate three times
(define d1 (diff e3 'x))
(define d2 (diff d1 'x))
(define d3 (diff d2 'x))

;; Show sizes
(display "── Expression sizes ──") (newline)
(display "  Expression       tree-size  dag-size") (newline)

(define (show-stats label expr)
  (let ((ts (tree-size expr))
        (ds (dag-size expr)))
    (display "  ") (display label)
    (display ts)
    (display "        ")
    (display ds)
    (newline)))

(show-stats "x^8            " e3)
(show-stats "d/dx(x^8)      " d1)
(show-stats "d^2/dx^2(x^8)  " d2)
(show-stats "d^3/dx^3(x^8)  " d3)
(newline)

;; Show naive vs let-inserted for d/dx
(display "── Naive serialization of d/dx(x^8) ──") (newline)
(display "  ") (write d1) (newline)
(newline)

;; Show let-inserted forms
(display "── Let-inserted forms ──") (newline)
(newline)

(display "x^8:") (newline)
(display "  ") (pp (let-insert e3)) (newline)
(newline)

(display "d/dx(x^8):") (newline)
(display "  ") (pp (let-insert d1)) (newline)
(newline)

(display "d^2/dx^2(x^8):") (newline)
(display "  ") (pp (let-insert d2)) (newline)
(newline)

(display "d^3/dx^3(x^8):") (newline)
(display "  ") (pp (let-insert d3)) (newline)
(newline)

;; Verify: d^k/dx^k(x^8) at x=2 should be 8!/(8-k)! * 2^(8-k)
(display "── Verification at x = 2 ──") (newline)
(define env (list (cons 'x 2)))
(display "  x^8      = ") (display (eval-expr e3 env))
(display "   (expected 256)") (newline)
(display "  d/dx     = ") (display (eval-expr d1 env))
(display "  (expected 1024)") (newline)
(display "  d^2/dx^2 = ") (display (eval-expr d2 env))
(display "  (expected 3584)") (newline)
(display "  d^3/dx^3 = ") (display (eval-expr d3 env))
(display " (expected 10752)") (newline)
(newline)

;; Show eq?-sharing: subexpressions are pointer-identical
(display "── Hash-consing sharing ──") (newline)
(display "  e1 = (* x x) appears in both e2's children: ")
(display (eq? (cadr e2) (caddr e2))) (newline)
(display "  diff(e1) and the (+ x x) inside d1 share nodes: ")
(let ((de1 (diff e1 'x)))
  (display (eq? de1 (list '+ 'x 'x)))) (newline)
(newline)

(display "── Heap ──") (newline)
(display "  Heap size: ") (display (heap-size)) (newline)
(gc)
(display "  After GC:  ") (display (heap-size)) (newline)
