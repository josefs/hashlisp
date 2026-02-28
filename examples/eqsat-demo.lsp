;;; ==========================================================================
;;; eqsat-demo.lsp — Equality Saturation Demo
;;; ==========================================================================
;;;
;;; Demonstrates equality saturation on arithmetic expressions.
;;; Shows how rewrite rules discover many equivalent forms of an
;;; expression, and how extraction finds the simplest one.

(load "examples/eqsat.lsp")

(display "=== Equality Saturation Demo ===") (newline) (newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 1: Arithmetic simplification
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 1: Arithmetic Simplification") (newline)
(display "  Expression: (* (+ a 0) 1)") (newline)
(display "  Expected simplification: a") (newline) (newline)

(eg-reset!)

;; Add the initial expression
(define root (eg-add-term '(* (+ a 0) 1)))

(display "  E-graph after adding term:") (newline)
(display "    Classes: ") (display (eg-num-classes)) (newline)
(display "    E-nodes: ") (display (eg-num-enodes)) (newline)

;; Define rewrite rules
(define arithmetic-rules
  (list
    ;; Additive identity
    (make-rule '(+ (? a) 0) '(? a))
    (make-rule '(+ 0 (? a)) '(? a))
    ;; Multiplicative identity
    (make-rule '(* (? a) 1) '(? a))
    (make-rule '(* 1 (? a)) '(? a))
    ;; Multiplicative annihilation
    (make-rule '(* (? a) 0) '0)
    (make-rule '(* 0 (? a)) '0)
    ;; Commutativity
    (make-rule '(+ (? a) (? b)) '(+ (? b) (? a)))
    (make-rule '(* (? a) (? b)) '(* (? b) (? a)))))

;; Run equality saturation
(display "  Running equality saturation...") (newline)
(define iters (eg-saturate! arithmetic-rules 10))

(display "  E-graph after saturation:") (newline)
(display "    Classes: ") (display (eg-num-classes)) (newline)
(display "    E-nodes: ") (display (eg-num-enodes)) (newline)

;; Extract the simplest equivalent form
(define simplified (eg-extract root))
(display "  Extracted: ") (display simplified) (newline)
(display "  (Simplified from (* (+ a 0) 1) to ") (display simplified)
(display ")") (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 2: Algebraic equivalences with associativity
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 2: Discovering Algebraic Equivalences") (newline)
(display "  Expression: (+ (+ a b) c)") (newline)
(display "  With associativity and commutativity rules") (newline) (newline)

(eg-reset!)

(define root2 (eg-add-term '(+ (+ a b) c)))

(define assoc-rules
  (list
    ;; Commutativity
    (make-rule '(+ (? a) (? b)) '(+ (? b) (? a)))
    ;; Associativity
    (make-rule '(+ (+ (? a) (? b)) (? c)) '(+ (? a) (+ (? b) (? c))))))

(display "  Running equality saturation...") (newline)
(eg-saturate! assoc-rules 10)

(display "  E-graph after saturation:") (newline)
(display "    Classes: ") (display (eg-num-classes)) (newline)
(display "    E-nodes: ") (display (eg-num-enodes)) (newline)

;; Show that several different orderings are all equivalent
(define root2a (eg-add-term '(+ a (+ b c))))
(define root2b (eg-add-term '(+ c (+ b a))))
(define root2c (eg-add-term '(+ (+ c a) b)))

(display "  All equivalent (same e-class via eq? on find):") (newline)
(display "    (+ (+ a b) c) ~ (+ a (+ b c)):  ")
(display (= (eg-find root2) (eg-find root2a))) (newline)
(display "    (+ (+ a b) c) ~ (+ c (+ b a)):  ")
(display (= (eg-find root2) (eg-find root2b))) (newline)
(display "    (+ (+ a b) c) ~ (+ (+ c a) b):  ")
(display (= (eg-find root2) (eg-find root2c))) (newline)

(define extracted2 (eg-extract root2))
(display "  Smallest: ") (display extracted2) (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 3: Strength reduction
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 3: Strength Reduction") (newline)
(display "  Expression: (* (* x 2) 2)") (newline)
(display "  With rule: (* (? a) 2) --> (shl (? a) 1)") (newline) (newline)

(eg-reset!)

(define root3 (eg-add-term '(* (* x 2) 2)))

(define strength-rules
  (list
    ;; Strength reduction: multiply by 2 → shift left
    (make-rule '(* (? a) 2) '(shl (? a) 1))
    (make-rule '(* 2 (? a)) '(shl (? a) 1))
    ;; Commutativity
    (make-rule '(* (? a) (? b)) '(* (? b) (? a)))))

(display "  Running equality saturation...") (newline)
(eg-saturate! strength-rules 10)

(display "  E-graph after saturation:") (newline)
(display "    Classes: ") (display (eg-num-classes)) (newline)
(display "    E-nodes: ") (display (eg-num-enodes)) (newline)

(define extracted3 (eg-extract root3))
(display "  Extracted: ") (display extracted3) (newline)
(display "  (Both (* ...) and (shl ...) forms are in the same class)") (newline)

;; Verify the shift form is there by adding it and checking equivalence
(define shift-form (eg-add-term '(shl (shl x 1) 1)))
(display "  (* (* x 2) 2) ~ (shl (shl x 1) 1): ")
(display (= (eg-find root3) (eg-find shift-form))) (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 4: Double negation and involution
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 4: Logical Simplification") (newline)
(display "  Expression: (neg (neg (neg (neg x))))") (newline) (newline)

(eg-reset!)

(define root4 (eg-add-term '(neg (neg (neg (neg x))))))

(define neg-rules
  (list
    ;; Double negation elimination
    (make-rule '(neg (neg (? a))) '(? a))))

(display "  Running equality saturation...") (newline)
(eg-saturate! neg-rules 10)

(define extracted4 (eg-extract root4))
(display "  Extracted: ") (display extracted4) (newline)
(display "  (Eliminated 4 negations!)") (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 5: Hash-consing advantage
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 5: Hash-Consing Advantage") (newline)
(display "  The e-graph internally creates many e-nodes.") (newline)
(display "  Hash-consing ensures identical e-nodes are shared:") (newline)
(newline)

(eg-reset!)
(define root5 (eg-add-term '(+ (* a b) (* b a))))

(define demo5-rules
  (list
    (make-rule '(* (? a) (? b)) '(* (? b) (? a)))
    (make-rule '(+ (? a) (? b)) '(+ (? b) (? a)))
    (make-rule '(+ (? a) (? a)) '(* 2 (? a)))))

(display "  Running equality saturation on (+ (* a b) (* b a))...") (newline)
(eg-saturate! demo5-rules 10)

(display "  After saturation:") (newline)
(display "    Classes: ") (display (eg-num-classes)) (newline)
(display "    E-nodes: ") (display (eg-num-enodes)) (newline)

;; The key insight: (* a b) and (* b a) are in the same class
(define ab (eg-add-term '(* a b)))
(define ba (eg-add-term '(* b a)))
(display "    (* a b) and (* b a) same class: ")
(display (= (eg-find ab) (eg-find ba))) (newline)

;; And (+ X X) = (* 2 X) was discovered
(define extracted5 (eg-extract root5))
(display "  Extracted: ") (display extracted5) (newline)
(display "  (Discovered that (+ X X) = (* 2 X) after commutativity!)") (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Stats
;;; ══════════════════════════════════════════════════════════════════

(display "Heap size: ") (display (heap-size)) (newline)
(gc)
(display "Heap size after GC: ") (display (heap-size)) (newline)
