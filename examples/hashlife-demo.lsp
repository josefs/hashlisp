;;; ==========================================================================
;;; hashlife-demo.lsp — HashLife Demo
;;; ==========================================================================
;;;
;;; Demonstrates Gosper's HashLife algorithm running in Hashlisp.
;;; The hash-consed heap acts as the quadtree unique table for free,
;;; and define-memo provides the RESULT cache.

(load "examples/hashlife.lsp")

(display "=== HashLife Demo ===") (newline) (newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 1: Blinker (period-2 oscillator)
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 1: Blinker (period-2 oscillator)") (newline) (newline)

(define blinker (make-blinker))
(display "  Generation 0:") (newline)
(display-region blinker -3 -3 3 3)

(define blinker-1 (hl-step-1 blinker))
(display "  Generation 1 (single step):") (newline)
(display-region blinker-1 -3 -3 3 3)

(define blinker-2 (hl-step-1 blinker-1))
(display "  Generation 2 (back to original):") (newline)
(display-region blinker-2 -3 -3 3 3)

(display "  Gen 0 and Gen 2 are eq? (hash-consing!): ")
(display (eq? blinker blinker-2)) (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 2: Glider
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 2: Glider") (newline) (newline)

(define glider (make-glider))
(display "  Generation 0:") (newline)
(display-region glider -5 -5 6 6)

;; Step the glider one generation at a time
(define glider-1 (hl-step-1 glider))
(display "  Generation 1:") (newline)
(display-region glider-1 -5 -5 6 6)

(define glider-2 (hl-step-1 glider-1))
(display "  Generation 2:") (newline)
(display-region glider-2 -5 -5 6 6)

(define glider-3 (hl-step-1 glider-2))
(display "  Generation 3:") (newline)
(display-region glider-3 -5 -5 6 6)

(define glider-4 (hl-step-1 glider-3))
(display "  Generation 4 (shifted 1 cell SE):") (newline)
(display-region glider-4 -5 -5 6 6)

(display "  Population gen 0: ") (display (node-population glider)) (newline)
(display "  Population gen 4: ") (display (node-population glider-4)) (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 3: R-pentomino (exponential speedup showcase)
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 3: R-pentomino") (newline)
(display "  This chaotic pattern takes ~1100 generations to stabilize.") (newline)
(display "  HashLife handles the exponential blowup via memoization.") (newline) (newline)

(define rpent (make-r-pentomino))

(display "  Generation 0, population: ") (display (node-population rpent)) (newline)
(display-region rpent -4 -4 4 4)

;; Use HashLife macro-steps for exponential time-skipping
;; Each hl-step on a level-k tree advances 2^(k-2) steps.
;; After two expands, level goes from 3 to 5 → advance by 2^3 = 8 gens.
;; Second call: level 5 → 7 after expand → advance by 2^5 = 32 gens.
;; The growing levels mean each step covers exponentially more time!

(define rpent-1 (hl-step rpent))
(display "  After macro-step 1, population: ") (display (node-population rpent-1)) (newline)

(define rpent-2 (hl-step rpent-1))
(display "  After macro-step 2, population: ") (display (node-population rpent-2)) (newline)

(define rpent-3 (hl-step rpent-2))
(display "  After macro-step 3, population: ") (display (node-population rpent-3)) (newline)
(display "  Level: ") (display (node-level rpent-3)) (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 4: Hash-consing advantage — canonical empty space
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 4: Hash-Consing Advantage") (newline) (newline)

(display "  Empty space is shared across the entire universe:") (newline)
(define e1 (empty-node 10))  ; 1024×1024 empty region
(define e2 (empty-node 10))  ; another one
(display "    Two 1024×1024 empty regions are eq?: ")
(display (eq? e1 e2)) (newline)

(display "  Empty node level 10 population: ")
(display (node-population e1)) (newline)

;; Show that sub-nodes are shared
(display "  Level-10 nw == level-10 ne (same empty sub-tree): ")
(display (eq? (node-nw e1) (node-ne e1))) (newline)

;; After evolving the R-pentomino, vast empty regions are all
;; represented by the same shared node
(display "  After R-pentomino evolution:") (newline)
(display "    Most of the universe is shared empty space.") (newline)
(display "    The tree-level is ") (display (node-level rpent-3))
(display " (representing a 2^")
(display (node-level rpent-3))
(display " × 2^")
(display (node-level rpent-3))
(display " grid)") (newline)
(display "    But only ") (display (node-population rpent-3))
(display " cells are alive!") (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Demo 5: Acorn — long-running pattern
;;; ══════════════════════════════════════════════════════════════════

(display "Demo 5: Acorn (stabilizes after 5206 generations)") (newline) (newline)

(define acorn (make-acorn))
(display "  Generation 0, population: ") (display (node-population acorn))
(newline)

;; Advance through several macro-steps
(define (advance-n root times)
  (if (= times 0) root
      (advance-n (hl-step root) (- times 1))))

(define acorn-1 (advance-n acorn 4))

(display "  After 4 HashLife macro-steps:") (newline)
(display "  Population: ") (display (node-population acorn-1)) (newline)
(display "  Level: ") (display (node-level acorn-1)) (newline)
(newline)


;;; ══════════════════════════════════════════════════════════════════
;;; Stats
;;; ══════════════════════════════════════════════════════════════════

(display "Heap statistics:") (newline)
(display "  Heap size: ") (display (heap-size)) (newline)
(gc)
(display "  After GC:  ") (display (heap-size)) (newline)
(display "  (Hash-consing means identical quadtree sub-regions") (newline)
(display "   are stored once. define-memo means identical sub-evolutions") (newline)
(display "   are computed once.)") (newline)
