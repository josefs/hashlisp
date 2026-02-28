;;; ==========================================================================
;;; ctl.lsp — CTL Model Checker (BDD-based)
;;; ==========================================================================
;;;
;;; A symbolic CTL model checker built on the BDD library.
;;; States are encoded as boolean variable vectors; the transition
;;; relation and state sets are BDDs.  All CTL operators are
;;; computed via fixed-point iteration over BDD operations.
;;;
;;; Hash-consing provides:
;;;   - Free BDD unique table (from bdd.lsp)
;;;   - Memoized BDD operations via define-memo
;;;   - O(1) fixed-point convergence check: old eq? new
;;;
;;; Usage:  (load "examples/ctl.lsp")

(load "examples/bdd.lsp")

;;; ── Kripke Structure ─────────────────────────────────────────────
;;;
;;; A model is: (n-vars . (trans . init))
;;;   n-vars : number of state bits
;;;   trans  : BDD over vars 1..n (current) and n+1..2n (next)
;;;   init   : BDD over vars 1..n (initial states)
;;;
;;; Variable convention:
;;;   Current state: variables 1, 2, ..., n
;;;   Next state:    variables n+1, n+2, ..., 2n
;;;
;;; An atomic proposition is a BDD over variables 1..n.

(define (make-model n-vars trans init)
  (cons n-vars (cons trans init)))

(define (model-nvars m) (car m))
(define (model-trans m) (cadr m))
(define (model-init m)  (cddr m))

;; The list of next-state variables for a model with n state bits
(define (next-vars n)
  (let loop ((i 1) (acc '()))
    (if (> i n) acc
        (loop (+ i 1) (cons (+ n i) acc)))))

;; The list of current-state variables
(define (curr-vars n)
  (let loop ((i 1) (acc '()))
    (if (> i n) acc
        (loop (+ i 1) (cons i acc)))))

;;; ── Variable Renaming ────────────────────────────────────────────
;;;
;;; To compute preimage, we need to:
;;;   1. Take a state-set BDD over current vars 1..n
;;;   2. Shift it to next vars n+1..2n
;;;   3. Conjoin with the transition relation
;;;   4. Existentially quantify out the next vars

;; Shift a BDD from current vars (1..n) to next vars (n+1..2n)
(define (bdd-shift-to-next b n)
  (let loop ((i n) (acc b))
    (if (< i 1) acc
        (loop (- i 1) (bdd-compose acc i (bdd-var-node (+ n i)))))))

;; Shift a BDD from next vars (n+1..2n) to current vars (1..n)
(define (bdd-shift-to-curr b n)
  (let loop ((i n) (acc b))
    (if (< i 1) acc
        (loop (- i 1) (bdd-compose acc (+ n i) (bdd-var-node i))))))

;;; ── Preimage (EX) ───────────────────────────────────────────────
;;;
;;; EX(φ) = { s | ∃s'. T(s,s') ∧ φ(s') }
;;;
;;; 1. Shift φ from current vars to next vars
;;; 2. Conjoin with transition relation T
;;; 3. Existentially quantify out all next-state variables

(define (ctl-ex model phi)
  (let* ((n (model-nvars model))
         (T (model-trans model))
         (phi-next (bdd-shift-to-next phi n))
         (conj (bdd-and T phi-next)))
    (bdd-exists* (next-vars n) conj)))

;;; ── AX ──────────────────────────────────────────────────────────
;;;
;;; AX(φ) = ¬EX(¬φ)

(define (ctl-ax model phi)
  (bdd-not (ctl-ex model (bdd-not phi))))

;;; ── Fixed-Point Iterations ──────────────────────────────────────
;;;
;;; The key insight: convergence check is eq? — a free pointer
;;; comparison thanks to hash-consing.  When the BDD doesn't change
;;; between iterations, we get the exact same hash-consed node.

;; Least fixed point: start from ⊥(#f), iterate f until stable
(define (lfp model f)
  (let loop ((z #f))
    (let ((z-next (f z)))
      (if (eq? z z-next) z          ;; free convergence check!
          (loop z-next)))))

;; Greatest fixed point: start from ⊤(#t), iterate f until stable
(define (gfp model f)
  (let loop ((z #t))
    (let ((z-next (f z)))
      (if (eq? z z-next) z
          (loop z-next)))))

;;; ── CTL Temporal Operators ──────────────────────────────────────

;; EF(φ) = μZ. φ ∨ EX(Z)    (least fixpoint)
(define (ctl-ef model phi)
  (lfp model (lambda (z) (bdd-or phi (ctl-ex model z)))))

;; EG(φ) = νZ. φ ∧ EX(Z)    (greatest fixpoint)
(define (ctl-eg model phi)
  (gfp model (lambda (z) (bdd-and phi (ctl-ex model z)))))

;; AF(φ) = μZ. φ ∨ AX(Z)    (least fixpoint)
(define (ctl-af model phi)
  (lfp model (lambda (z) (bdd-or phi (ctl-ax model z)))))

;; AG(φ) = νZ. φ ∧ AX(Z)    (greatest fixpoint)
(define (ctl-ag model phi)
  (gfp model (lambda (z) (bdd-and phi (ctl-ax model z)))))

;; E[φ U ψ] = μZ. ψ ∨ (φ ∧ EX(Z))
(define (ctl-eu model phi psi)
  (lfp model (lambda (z) (bdd-or psi (bdd-and phi (ctl-ex model z))))))

;; A[φ U ψ] = μZ. ψ ∨ (φ ∧ AX(Z))
(define (ctl-au model phi psi)
  (lfp model (lambda (z) (bdd-or psi (bdd-and phi (ctl-ax model z))))))

;;; ── Forward Image (Post) ─────────────────────────────────────────
;;;
;;; post(φ) = { s' | ∃s. T(s,s') ∧ φ(s) }
;;;
;;; 1. Conjoin φ (over current vars) with transition relation T
;;; 2. Existentially quantify out all current-state variables
;;; 3. Shift result from next vars to current vars

(define (ctl-post model phi)
  (let* ((n (model-nvars model))
         (T (model-trans model))
         (conj (bdd-and T phi))
         (img (bdd-exists* (curr-vars n) conj)))
    (bdd-shift-to-curr img n)))

;;; ── Reachability ────────────────────────────────────────────────

;; Compute the set of all reachable states from init (forward)
(define (reachable model)
  (lfp model (lambda (z) (bdd-or (model-init model) (ctl-post model z)))))

;;; ── Checking ────────────────────────────────────────────────────

;; Check: does every initial state satisfy φ?
(define (ctl-check model phi)
  (eq? (bdd-and (model-init model) (bdd-not phi)) #f))

;; Check with reachability: does every reachable state satisfy φ?
(define (ctl-check-reachable model phi)
  (let ((reach (reachable model)))
    (eq? (bdd-and reach (bdd-not phi)) #f)))

;;; ── Model Construction Helpers ──────────────────────────────────

;; Build a transition relation from a list of transition rules.
;; Each rule is a BDD over current (1..n) and next (n+1..2n) vars
;; representing a set of transitions.  The full relation is their
;; disjunction.
(define (transitions->bdd rules)
  (fold (lambda (r acc) (bdd-or r acc)) #f rules))

;; A transition where variable i (current) maps to variable j (next):
;; next_j ↔ f(current), and all other next vars stay the same.
;; "frame" encodes that unchanged variables keep their value.
(define (frame-for n changed-vars)
  (let loop ((i 1) (acc #t))
    (if (> i n) acc
        (if (member i changed-vars)
            (loop (+ i 1) acc)
            ;; next_i ↔ curr_i
            (loop (+ i 1)
                  (bdd-and acc
                           (bdd-iff (bdd-var-node (+ n i))
                                    (bdd-var-node i))))))))

(define (member x lst)
  (cond ((null? lst) #f)
        ((= x (car lst)) #t)
        (else (member x (cdr lst)))))

;; Guard-action transition: if guard(current), then next = action
;; guard: BDD over 1..n
;; action-pairs: list of (var-index . bdd-for-next-value)
;; All vars not in action-pairs keep their current value (frame condition)
(define (guarded-transition n guard action-pairs)
  (let* ((changed (map car action-pairs))
         (fr (frame-for n changed))
         (acts (fold (lambda (pair acc)
                       (bdd-and acc
                                (bdd-iff (bdd-var-node (+ n (car pair)))
                                         (cdr pair))))
                     #t action-pairs)))
    (bdd-and guard (bdd-and acts fr))))

;;; ── Display Helpers ─────────────────────────────────────────────

;; Enumerate all satisfying assignments of a BDD (up to n vars)
(define (bdd-sat-all b n)
  (define (walk b var env)
    (cond
      ((eq? b #f) '())
      ((> var n) (if (eq? b #t) (list (reverse env)) '()))
      ((eq? b #t)
       ;; Remaining variables are free — enumerate both values
       (append (walk #t (+ var 1) (cons (cons var #f) env))
               (walk #t (+ var 1) (cons (cons var #t) env))))
      ((= (bdd-var b) var)
       (append (walk (bdd-low b) (+ var 1) (cons (cons var #f) env))
               (walk (bdd-high b) (+ var 1) (cons (cons var #t) env))))
      (else
       ;; Variable not in BDD — free, enumerate both
       (append (walk b (+ var 1) (cons (cons var #f) env))
               (walk b (+ var 1) (cons (cons var #t) env))))))
  (walk b 1 '()))

;; Print a state set with symbolic names
(define (print-states b n var-names)
  (let ((states (bdd-sat-all b n)))
    (for-each (lambda (state)
      (display "    ")
      (for-each (lambda (binding)
        (let ((name (list-ref var-names (- (car binding) 1))))
          (display name) (display "=")
          (display (if (cdr binding) 1 0))
          (display " ")))
        state)
      (newline))
      states)))

(define (list-ref lst i)
  (if (= i 0) (car lst) (list-ref (cdr lst) (- i 1))))
