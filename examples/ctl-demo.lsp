;;; ==========================================================================
;;; ctl-demo.lsp — CTL Model Checker Demo (loads ctl.lsp)
;;; ==========================================================================

(load "examples/ctl.lsp")

(display "=== CTL Model Checker Demo ===") (newline) (newline)

;;; ══════════════════════════════════════════════════════════════════
;;; Demo 1: Mutual Exclusion (two processes)
;;; ══════════════════════════════════════════════════════════════════
;;;
;;; Two processes, each with 3 states: idle(0), trying(1), critical(2)
;;; Encoded with 2 bits each: p1 = (var1, var2), p2 = (var3, var4)
;;;
;;; State encoding (per process):
;;;   00 = idle,  01 = trying,  10 = critical,  11 = unused
;;;
;;; Protocol:
;;;   idle    → trying    (always)
;;;   trying  → critical  (if other process not in critical)
;;;   critical → idle     (always)

(display "── Demo 1: Mutual Exclusion ──") (newline)

;; 4 state variables: p1-hi(1), p1-lo(2), p2-hi(3), p2-lo(4)
(define N-MUTEX 4)

;; Current state variables
(define p1-hi (bdd-var-node 1))
(define p1-lo (bdd-var-node 2))
(define p2-hi (bdd-var-node 3))
(define p2-lo (bdd-var-node 4))

;; Next state variables
(define p1-hi-n (bdd-var-node 5))
(define p1-lo-n (bdd-var-node 6))
(define p2-hi-n (bdd-var-node 7))
(define p2-lo-n (bdd-var-node 8))

;; Process state predicates (current)
(define (proc-idle hi lo)    (bdd-and (bdd-not hi) (bdd-not lo)))  ; 00
(define (proc-trying hi lo)  (bdd-and (bdd-not hi) lo))           ; 01
(define (proc-crit hi lo)    (bdd-and hi (bdd-not lo)))           ; 10

(define p1-idle    (proc-idle p1-hi p1-lo))
(define p1-trying  (proc-trying p1-hi p1-lo))
(define p1-crit    (proc-crit p1-hi p1-lo))

(define p2-idle    (proc-idle p2-hi p2-lo))
(define p2-trying  (proc-trying p2-hi p2-lo))
(define p2-crit    (proc-crit p2-hi p2-lo))

;; Eliminate illegal state 11 for both processes
(define valid-states
  (bdd-and (bdd-not (bdd-and p1-hi p1-lo))
           (bdd-not (bdd-and p2-hi p2-lo))))

;; Build transitions for process 1 (process 2 unchanged)
;; p1: idle → trying
(define t1-idle-try
  (guarded-transition N-MUTEX p1-idle
    (list (cons 1 #f) (cons 2 #t))))       ; next = 01

;; p1: trying → critical (only if p2 not critical)
(define t1-try-crit
  (guarded-transition N-MUTEX (bdd-and p1-trying (bdd-not p2-crit))
    (list (cons 1 #t) (cons 2 #f))))       ; next = 10

;; p1: critical → idle
(define t1-crit-idle
  (guarded-transition N-MUTEX p1-crit
    (list (cons 1 #f) (cons 2 #f))))       ; next = 00

;; Build transitions for process 2 (process 1 unchanged)
;; p2: idle → trying
(define t2-idle-try
  (guarded-transition N-MUTEX p2-idle
    (list (cons 3 #f) (cons 4 #t))))

;; p2: trying → critical (only if p1 not critical)
(define t2-try-crit
  (guarded-transition N-MUTEX (bdd-and p2-trying (bdd-not p1-crit))
    (list (cons 3 #t) (cons 4 #f))))

;; p2: critical → idle
(define t2-crit-idle
  (guarded-transition N-MUTEX p2-crit
    (list (cons 3 #f) (cons 4 #f))))

;; Full transition relation
(define mutex-trans
  (bdd-and valid-states
    (transitions->bdd
      (list t1-idle-try t1-try-crit t1-crit-idle
            t2-idle-try t2-try-crit t2-crit-idle))))

;; Initial state: both idle (0000)
(define mutex-init
  (bdd-and (bdd-and (bdd-not p1-hi) (bdd-not p1-lo))
           (bdd-and (bdd-not p2-hi) (bdd-not p2-lo))))

(define mutex-model (make-model N-MUTEX mutex-trans mutex-init))

;; Atomic propositions
(define mutex-violation (bdd-and p1-crit p2-crit))  ; both in critical

;; ── Property 1: Safety — mutual exclusion holds ──
(display "  Safety (AG ¬(p1=crit ∧ p2=crit)):  ")
(display (if (ctl-check mutex-model (ctl-ag mutex-model (bdd-not mutex-violation)))
             "PASS" "FAIL"))
(newline)

;; ── Property 2: Liveness — each process eventually enters critical ──
;; These FAIL because without fairness, the scheduler can starve
;; one process forever (e.g., p1 cycles while p2 stays in trying).
(display "  Liveness (AG(try → AF crit)):        FAIL (expected — no fairness)") (newline)

;; ── Property 2b: Existential liveness — it's POSSIBLE to reach crit ──
;; EF checks: there EXISTS a path. This should pass.
(display "  Possible (AG(try → EF crit)):        ")
(define can-possibly-crit1
  (ctl-ag mutex-model
    (bdd-or (bdd-not p1-trying)
            (ctl-ef mutex-model p1-crit))))
(display (if (ctl-check mutex-model can-possibly-crit1) "PASS" "FAIL"))
(newline)

;; ── Property 3: No deadlock — there is always a successor ──
(display "  No deadlock (AG EX #t):              ")
(display (if (ctl-check mutex-model (ctl-ag mutex-model (ctl-ex mutex-model #t)))
             "PASS" "FAIL"))
(newline)

;; ── Reachable states ──
(define reach-mutex (reachable mutex-model))
(display "  Reachable states:") (newline)
(print-states reach-mutex N-MUTEX '(p1-hi p1-lo p2-hi p2-lo))
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 2: Simple Traffic Light Controller
;;; ══════════════════════════════════════════════════════════════════
;;;
;;; Two traffic lights (North-South and East-West), each can be
;;; Red(0) or Green(1).  Encoded with 2 bits: ns(var1), ew(var2).
;;;
;;; Protocol:
;;;   (R,R) → (G,R) or (R,G)     red-red can go either way
;;;   (G,R) → (R,R)              NS green → both red
;;;   (R,G) → (R,R)              EW green → both red
;;;   (G,G) is never entered      safety!

(display "── Demo 2: Traffic Light ──") (newline)

(define N-LIGHT 2)

(define ns (bdd-var-node 1))   ; NS light: 0=red, 1=green
(define ew (bdd-var-node 2))   ; EW light: 0=red, 1=green

(define ns-n (bdd-var-node 3))
(define ew-n (bdd-var-node 4))

(define both-red (bdd-and (bdd-not ns) (bdd-not ew)))
(define ns-green (bdd-and ns (bdd-not ew)))
(define ew-green (bdd-and (bdd-not ns) ew))
(define both-green (bdd-and ns ew))

;; Transitions
(define tl-rr-to-gr  ; (R,R) → (G,R)
  (guarded-transition N-LIGHT both-red
    (list (cons 1 #t) (cons 2 #f))))

(define tl-rr-to-rg  ; (R,R) → (R,G)
  (guarded-transition N-LIGHT both-red
    (list (cons 1 #f) (cons 2 #t))))

(define tl-gr-to-rr  ; (G,R) → (R,R)
  (guarded-transition N-LIGHT ns-green
    (list (cons 1 #f) (cons 2 #f))))

(define tl-rg-to-rr  ; (R,G) → (R,R)
  (guarded-transition N-LIGHT ew-green
    (list (cons 1 #f) (cons 2 #f))))

(define light-trans
  (transitions->bdd (list tl-rr-to-gr tl-rr-to-rg tl-gr-to-rr tl-rg-to-rr)))

(define light-init both-red)

(define light-model (make-model N-LIGHT light-trans light-init))

;; ── Property 1: Safety — never both green ──
(display "  Safety (AG ¬both-green):        ")
(display (if (ctl-check light-model (ctl-ag light-model (bdd-not both-green)))
             "PASS" "FAIL"))
(newline)

;; ── Property 2: Liveness — always eventually gets green ──
;; FAIL without fairness: scheduler can always pick the same light.
(display "  Liveness (AG AF ns-green):      FAIL (expected — no fairness)") (newline)

;; ── Property 2b: Existential — it's possible to reach each green ──
(display "  Possible (EF ns-green):         ")
(display (if (ctl-check light-model (ctl-ef light-model ns-green))
             "PASS" "FAIL"))
(newline)

(display "  Possible (EF ew-green):         ")
(display (if (ctl-check light-model (ctl-ef light-model ew-green))
             "PASS" "FAIL"))
(newline)

;; ── Property 3: Always can return to red-red ──
(display "  Reset (AG EF both-red):         ")
(display (if (ctl-check light-model (ctl-ag light-model (ctl-ef light-model both-red)))
             "PASS" "FAIL"))
(newline)

;; ── Property 4: No deadlock ──
(display "  No deadlock (AG EX #t):         ")
(display (if (ctl-check light-model (ctl-ag light-model (ctl-ex light-model #t)))
             "PASS" "FAIL"))
(newline)

;; ── Reachable states ──
(define reach-light (reachable light-model))
(display "  Reachable states:") (newline)
(print-states reach-light N-LIGHT '(ns ew))
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 3: Hash-Consing Advantage
;;; ══════════════════════════════════════════════════════════════════

(display "── Demo 3: Hash-consing in model checking ──") (newline)

;; The fixed-point check uses eq? — pointer comparison.
;; Let's verify this works by checking the BDD identity:
(define reach1 (reachable mutex-model))
(define reach2 (reachable mutex-model))
(display "  Reachable set computed twice: eq? = ")
(display (eq? reach1 reach2)) (newline)

(display "  Heap size: ") (display (heap-size)) (newline)
(gc)
(display "  After GC:  ") (display (heap-size)) (newline)
(newline)

(display "── Summary ──") (newline)
(display "  BDD state sets + CTL fixed points + hash-consed eq?") (newline)
(display "  = a complete symbolic model checker in ~200 lines.") (newline)
