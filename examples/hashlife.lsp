;;; ==========================================================================
;;; hashlife.lsp — Gosper's HashLife Algorithm
;;; ==========================================================================
;;;
;;; HashLife is an algorithm for simulating Conway's Game of Life that
;;; achieves exponential speedups by memoizing the future evolution of
;;; quadtree nodes. In conventional implementations, a hash table is
;;; used to both:
;;;   (a) Canonicalize  nodes (unique table / hash-consing), and
;;;   (b) Memoize the RESULT fquadtreeunction that computes future states.
;;;
;;; In Hashlisp, we get (a) for FREE from the hash-consed heap:
;;;   - A quadtree node is a pair ((nw . ne) . (sw . se)).
;;;   - Two nodes with identical children are eq? automatically.
;;;   - No explicit unique table needed!
;;;
;;; And we get (b) from define-memo:
;;;   - The RESULT function is a memoized function.
;;;   - Since arguments are hash-consed, identical quadtrees produce
;;;     the same memo key → automatic computation reuse.
;;;
;;; Representation:
;;;   Level 0 (leaf): 0 (dead) or 1 (alive)
;;;   Level k > 0:    ((nw . ne) . (sw . se)) where each is a level k-1 node
;;;
;;;   A level-k node represents a 2^k × 2^k grid.
;;;   The RESULT of a level-k node (k >= 2) is a level-(k-1) node
;;;   centered on the original, advanced by 2^(k-2) generations.
;;;
;;; Usage:  (load "examples/hashlife.lsp")

;;; ── Constructors ─────────────────────────────────────────────────

;; Leaf cells (level 0)
(define dead  0)
(define alive 1)

;; Make a level-k node from four level-(k-1) children.
;; Stored as ((nw . ne) . (sw . se)) — a balanced binary tree.
;; Hash-consing ensures identical quadtrees are always eq?.
;;
;; Bonus: the sub-pairs (nw . ne) and (sw . se) represent the
;; north and south halves of the grid. These are also hash-consed,
;; so nodes sharing the same north or south half get sub-structural
;; sharing for free — fewer heap objects than a flat list.
(define (make-node nw ne sw se)
  (cons (cons nw ne) (cons sw se)))

;;; ── Accessors (all O(1)) ─────────────────────────────────────────

(define (node-nw n) (caar n))
(define (node-ne n) (cdar n))
(define (node-sw n) (cadr n))
(define (node-se n) (cddr n))

;;; ── Level computation ────────────────────────────────────────────

(define (leaf? n) (number? n))

(define-memo (node-level n)
  (if (leaf? n) 0
      (+ 1 (node-level (node-nw n)))))

;;; ── Population count ─────────────────────────────────────────────

(define-memo (node-population n)
  (if (leaf? n) n
      (+ (node-population (node-nw n))
         (node-population (node-ne n))
         (node-population (node-sw n))
         (node-population (node-se n)))))

;;; ── Empty nodes (canonical) ──────────────────────────────────────
;;;
;;; An empty level-k node. Hash-consing ensures there's only one
;;; per level — all empty regions share the same identity.

(define-memo (empty-node level)
  (if (= level 0) dead
      (let ((sub (empty-node (- level 1))))
        (make-node sub sub sub sub))))

;;; ── Expand: grow the universe by one level ───────────────────────
;;;
;;; Wraps the current root in a larger node with empty borders.

(define (expand root)
  (let ((e (empty-node (- (node-level root) 1))))
    (make-node
      (make-node e e e (node-nw root))
      (make-node e e (node-ne root) e)
      (make-node e (node-sw root) e e)
      (make-node (node-se root) e e e))))

;;; ── Game of Life rule for a single cell ──────────────────────────
;;;
;;; Given 9 cells (center + 8 neighbors), compute next state.

(define (life-rule c n0 n1 n2 n3 n4 n5 n6 n7)
  (let ((neighbors (+ n0 n1 n2 n3 n4 n5 n6 n7)))
    (if (= c alive)
        (if (or (= neighbors 2) (= neighbors 3)) alive dead)
        (if (= neighbors 3) alive dead))))

;;; ── Slow step for level-2 nodes (base case) ─────────────────────
;;;
;;; A level-2 node is a 4×4 grid. We compute its RESULT: the 2×2
;;; center after one generation step.
;;;
;;; Layout of a level-2 node (each child is a level-1 = 2×2 node):
;;;
;;;   nw = (a b    ne = (c d
;;;         e f)          g h)
;;;   sw = (i j    se = (k l
;;;         m n)          o p)
;;;
;;; Forming a 4×4 grid:
;;;   a b | c d
;;;   e f | g h
;;;   ----+----
;;;   i j | k l
;;;   m n | o p
;;;
;;; We compute the 2×2 center (f' g' j' k') after one step.

(define (slow-step node)
  (let* ((nw (node-nw node)) (ne (node-ne node))
         (sw (node-sw node)) (se (node-se node))
         ;; Extract the 16 cells of the 4×4 grid
         (a (node-nw nw)) (b (node-ne nw)) (c (node-nw ne)) (d (node-ne ne))
         (e (node-sw nw)) (f (node-se nw)) (g (node-sw ne)) (h (node-se ne))
         (i (node-nw sw)) (j (node-ne sw)) (k (node-nw se)) (l (node-ne se))
         (m (node-sw sw)) (n (node-se sw)) (o (node-sw se)) (p (node-se se))
         ;; Compute next state for the inner 2×2
         (f2 (life-rule f a b c e g i j k))
         (g2 (life-rule g b c d f h j k l))
         (j2 (life-rule j e f g i k m n o))
         (k2 (life-rule k f g h j l n o p)))
    (make-node f2 g2 j2 k2)))

;;; ── RESULT: the core HashLife function ───────────────────────────
;;;
;;; For a level-k node (k >= 2), RESULT returns a level-(k-1) node
;;; representing the center of the universe advanced by 2^(k-2) steps.
;;;
;;; define-memo + hash-consed arguments = automatic memoization.
;;; This is the key insight: identical sub-universes evolve identically
;;; and we compute each evolution only once.

(define-memo (hl-result node)
  (let ((level (node-level node)))
    (if (= level 2)
        ;; Base case: 4×4 → 2×2 after 1 step
        (slow-step node)
        ;; Recursive case: compose 9 overlapping sub-results
        (let* ((nw (node-nw node)) (ne (node-ne node))
               (sw (node-sw node)) (se (node-se node))
               ;; 9 overlapping level-(k-1) sub-squares
               (n00 (hl-result nw))
               (n01 (hl-result (make-node (node-ne nw) (node-nw ne)
                                          (node-se nw) (node-sw ne))))
               (n02 (hl-result ne))
               (n10 (hl-result (make-node (node-sw nw) (node-se nw)
                                          (node-nw sw) (node-ne sw))))
               (n11 (hl-result (make-node (node-se nw) (node-sw ne)
                                          (node-ne sw) (node-nw se))))
               (n12 (hl-result (make-node (node-sw ne) (node-se ne)
                                          (node-nw se) (node-ne se))))
               (n20 (hl-result sw))
               (n21 (hl-result (make-node (node-ne sw) (node-nw se)
                                          (node-se sw) (node-sw se))))
               (n22 (hl-result se)))
          ;; Compose: 4 overlapping results → final result
          (make-node
            (hl-result (make-node n00 n01 n10 n11))
            (hl-result (make-node n01 n02 n11 n12))
            (hl-result (make-node n10 n11 n20 n21))
            (hl-result (make-node n11 n12 n21 n22)))))))

;;; ── Step: advance the universe ───────────────────────────────────
;;;
;;; To step by 2^(k-2) generations, first expand so there's room
;;; for the result, then call hl-result.

(define (hl-step root)
  ;; Expand twice to ensure the result fits
  (let* ((padded (expand (expand root)))
         (result (hl-result padded)))
    result))

;;; ── Single-generation step (brute force for small demos) ─────────
;;;
;;; Steps exactly 1 generation by extracting cells, applying rules,
;;; and rebuilding. Useful for demonstrating oscillators.

(define (hl-step-1 root)
  (let* ((cells (tree->cells root))
         (next-cells (life-step-cells cells)))
    (if (null? next-cells)
        (empty-node 2)
        (cells->tree next-cells))))

;; Apply Game of Life rules to a cell list, returning new cell list
(define (life-step-cells cells)
  (let* ((cell-set cells)
         ;; Gather all candidate cells (alive + their neighbors)
         (candidates (gather-candidates cells)))
    ;; For each candidate, count neighbors and apply rule
    (filter (lambda (c) (not (not c)))
            (map (lambda (pos)
                   (let* ((x (car pos)) (y (cdr pos))
                          (n (count-neighbors x y cell-set))
                          (is-alive (cell-member? x y cell-set)))
                     (if is-alive
                         (if (or (= n 2) (= n 3)) pos #f)
                         (if (= n 3) pos #f))))
                 candidates))))

;; Check if a cell is in the set
(define (cell-member? x y cells)
  (cond ((null? cells) #f)
        ((and (= x (caar cells)) (= y (cdar cells))) #t)
        (else (cell-member? x y (cdr cells)))))

;; Count live neighbors of (x, y)
(define (count-neighbors x y cells)
  (let loop ((dx -1) (count 0))
    (if (> dx 1) count
        (let iloop ((dy -1) (c count))
          (if (> dy 1) (loop (+ dx 1) c)
              (if (and (= dx 0) (= dy 0))
                  (iloop (+ dy 1) c)
                  (iloop (+ dy 1)
                         (if (cell-member? (+ x dx) (+ y dy) cells)
                             (+ c 1) c))))))))

;; Gather all unique candidate positions (alive cells + their neighbors)
(define (gather-candidates cells)
  (let ((result '()))
    (for-each
      (lambda (cell)
        (let ((cx (car cell)) (cy (cdr cell)))
          (let dxloop ((dx -1))
            (if (> dx 1) 'done
                (begin
                  (let dyloop ((dy -1))
                    (if (> dy 1) 'done
                        (begin
                          (let ((nx (+ cx dx)) (ny (+ cy dy)))
                            (if (not (cell-member? nx ny result))
                                (set! result (cons (cons nx ny) result))))
                          (dyloop (+ dy 1)))))
                  (dxloop (+ dx 1)))))))
      cells)
    result))

;;; ── Multi-step: advance by a specific power-of-2 ────────────────
;;;
;;; hl-step on a level-k root advances by 2^(k-2) generations.
;;; To advance by more, we expand to a larger level and step.

(define (hl-advance root generations)
  ;; Ensure the root is large enough: level >= log2(gens) + 2
  ;; Then step
  (let ((padded (ensure-level root (+ (log2-ceil generations) 3))))
    (let ((result (hl-result padded)))
      ;; Trim empty borders
      (trim result))))

;; Ensure root is at least the given level
(define (ensure-level root min-level)
  (if (>= (node-level root) min-level) root
      (ensure-level (expand root) min-level)))

;; Integer log2 (ceiling)
(define (log2-ceil n)
  (let loop ((k 0) (p 1))
    (if (>= p n) k
        (loop (+ k 1) (* p 2)))))

;; Trim: remove empty borders
(define (trim node)
  (if (leaf? node) node
      (if (<= (node-level node) 2) node
          (let* ((nw (node-nw node)) (ne (node-ne node))
                 (sw (node-sw node)) (se (node-se node)))
            ;; Check if outer ring is all empty
            (if (and (= 0 (node-population (node-nw nw)))
                     (= 0 (node-population (node-ne nw)))
                     (= 0 (node-population (node-sw nw)))
                     (= 0 (node-population (node-nw ne)))
                     (= 0 (node-population (node-ne ne)))
                     (= 0 (node-population (node-se ne)))
                     (= 0 (node-population (node-nw sw)))
                     (= 0 (node-population (node-sw sw)))
                     (= 0 (node-population (node-se sw)))
                     (= 0 (node-population (node-sw se)))
                     (= 0 (node-population (node-ne se)))
                     (= 0 (node-population (node-se se))))
                ;; Inner region = center = result-sized
                (trim (make-node (node-se nw) (node-sw ne)
                                 (node-ne sw) (node-nw se)))
                node)))))

;;; ── Build patterns from cell lists ───────────────────────────────
;;;
;;; Convert a list of (x . y) live cell coordinates into a quadtree.
;;; Coordinates are relative to (0,0) at the center.

(define (cells->tree cells)
  (if (null? cells)
      (make-node dead dead dead dead)  ; level 1 empty
      (let* ((bounds (cell-bounds cells))
             (min-x (car bounds)) (max-x (cadr bounds))
             (min-y (caddr bounds)) (max-y (car (cdr (cddr bounds))))
             (span (max (- max-x min-x) (- max-y min-y)))
             (level (max 2 (+ 1 (log2-ceil (+ span 1)))))
             (size (expt 2 level))
             (half (/ size 2)))
        ;; Build by inserting cells into an empty tree
        (let loop ((cs cells) (tree (empty-node level)))
          (if (null? cs) tree
              (loop (cdr cs)
                    (tree-set tree level
                              (+ (caar cs) half)
                              (+ (cdar cs) half)
                              alive)))))))

(define (cell-bounds cells)
  (let loop ((cs cells) (min-x 0) (max-x 0) (min-y 0) (max-y 0))
    (if (null? cs)
        (list min-x max-x min-y max-y)
        (let ((x (caar cs)) (y (cdar cs)))
          (loop (cdr cs)
                (min x min-x) (max x max-x)
                (min y min-y) (max y max-y))))))

;;; ── Set a cell in a quadtree ─────────────────────────────────────

(define (tree-set node level x y val)
  (if (= level 0) val
      (let* ((half (expt 2 (- level 1)))
             (west (< x half))
             (north (< y half))
             (sub-x (if west x (- x half)))
             (sub-y (if north y (- y half))))
        (cond
          ((and north west)
           (make-node (tree-set (node-nw node) (- level 1) sub-x sub-y val)
                      (node-ne node) (node-sw node) (node-se node)))
          ((and north (not west))
           (make-node (node-nw node)
                      (tree-set (node-ne node) (- level 1) sub-x sub-y val)
                      (node-sw node) (node-se node)))
          ((and (not north) west)
           (make-node (node-nw node) (node-ne node)
                      (tree-set (node-sw node) (- level 1) sub-x sub-y val)
                      (node-se node)))
          (else
           (make-node (node-nw node) (node-ne node) (node-sw node)
                      (tree-set (node-se node) (- level 1) sub-x sub-y val)))))))

;;; ── Get a cell from a quadtree ───────────────────────────────────

(define (tree-get node level x y)
  (if (= level 0) node
      (let* ((half (expt 2 (- level 1)))
             (west (< x half))
             (north (< y half))
             (sub-x (if west x (- x half)))
             (sub-y (if north y (- y half))))
        (cond
          ((and north west)       (tree-get (node-nw node) (- level 1) sub-x sub-y))
          ((and north (not west)) (tree-get (node-ne node) (- level 1) sub-x sub-y))
          ((and (not north) west) (tree-get (node-sw node) (- level 1) sub-x sub-y))
          (else                   (tree-get (node-se node) (- level 1) sub-x sub-y))))))

;;; ── Extract live cells ───────────────────────────────────────────

(define (tree->cells node)
  (let ((level (node-level node))
        (size (expt 2 (node-level node)))
        (half (expt 2 (- (node-level node) 1))))
    (let ((cells '()))
      (define (walk n lev ox oy)
        (cond
          ((= lev 0)
           (if (= n alive)
               (set! cells (cons (cons (- ox half) (- oy half)) cells))))
          ((= 0 (node-population n))
           'skip)
          (else
           (let ((h (expt 2 (- lev 1))))
             (walk (node-nw n) (- lev 1) ox oy)
             (walk (node-ne n) (- lev 1) (+ ox h) oy)
             (walk (node-sw n) (- lev 1) ox (+ oy h))
             (walk (node-se n) (- lev 1) (+ ox h) (+ oy h))))))
      (walk node level 0 0)
      cells)))

;;; ── Display a region as ASCII ────────────────────────────────────

(define (display-region node x0 y0 x1 y1)
  (let* ((level (node-level node))
         (size (expt 2 level))
         (half (/ size 2)))
    (let yloop ((y y0))
      (if (> y y1) 'done
          (begin
            (let xloop ((x x0))
              (if (> x x1) 'done
                  (begin
                    (let ((tx (+ x half)) (ty (+ y half)))
                      (if (and (>= tx 0) (< tx size) (>= ty 0) (< ty size))
                          (if (= (tree-get node level tx ty) alive)
                              (display "#")
                              (display "."))
                          (display ".")))
                    (xloop (+ x 1)))))
            (newline)
            (yloop (+ y 1)))))))

;;; ── Well-known patterns ──────────────────────────────────────────

;; Glider (period-4, moves SE)
(define (make-glider)
  (cells->tree '((0 . -1) (1 . 0) (-1 . 1) (0 . 1) (1 . 1))))

;; Blinker (period-2 oscillator)
(define (make-blinker)
  (cells->tree '((-1 . 0) (0 . 0) (1 . 0))))

;; R-pentomino (chaotic for ~1100 generations)
(define (make-r-pentomino)
  (cells->tree '((0 . -1) (1 . -1) (-1 . 0) (0 . 0) (0 . 1))))

;; Acorn (takes 5206 generations to stabilize)
(define (make-acorn)
  (cells->tree '((-2 . -1) (0 . 0) (-3 . 1) (-2 . 1)
                 (1 . 1) (2 . 1) (3 . 1))))

;; Lightweight spaceship (LWSS)
(define (make-lwss)
  (cells->tree '((0 . 0) (3 . 0) (4 . 1) (0 . 2) (4 . 2)
                 (1 . 3) (2 . 3) (3 . 3) (4 . 3))))
