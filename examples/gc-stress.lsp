;;; ==========================================================================
;;; gc-stress.lsp — Stress test for the garbage collector
;;; ==========================================================================
;;; Creates lots of garbage and demonstrates that GC works.

(display "=== GC Stress Test ===") (newline) (newline)

(display "Initial heap size: ") (display (heap-size)) (newline)

;; Create a bunch of cons cells in a loop that become garbage
(define (make-garbage n)
  (let loop ((i 0))
    (when (< i n)
      ;; These temporary lists become garbage immediately
      (list i (+ i 1) (+ i 2) (+ i 3) (+ i 4))
      (loop (+ i 1)))))

(display "Creating 1000 temporary lists...") (newline)
(make-garbage 1000)
(display "Heap size before GC: ") (display (heap-size)) (newline)

(define swept (gc))
(display "GC swept: ") (display swept) (display " objects") (newline)
(display "Heap size after GC: ") (display (heap-size)) (newline)
(newline)

;; Now show that live data is preserved
(define keeper (list 1 2 3 4 5))
(display "Kept list: ") (display keeper) (newline)

(make-garbage 500)
(display "After more garbage, heap: ") (display (heap-size)) (newline)
(gc)
(display "After GC, heap: ") (display (heap-size)) (newline)
(display "Kept list still alive: ") (display keeper) (newline)
(newline)

;; Hash-consing means some "garbage" might actually be shared with live data!
(display "--- Hash-consing + GC interaction ---") (newline)
(define shared (list 1 2 3))
(define temp   (cons 99 (list 1 2 3)))  ; (list 1 2 3) shares with `shared`
(display "shared = ") (display shared) (newline)
(display "temp = ") (display temp) (newline)
(display "(eq? (cdr temp) shared) = ") (display (eq? (cdr temp) shared)) (newline)
(display "  The tail of temp IS shared -- hash-consing in action!") (newline)
