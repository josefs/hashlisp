;;; ==========================================================================
;;; mergesort.lsp — Merge sort, showcasing list manipulation
;;; ==========================================================================

(display "=== Merge Sort ===") (newline) (newline)

(define (take lst n)
  (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (= n 0) (null? lst)) lst
      (drop (cdr lst) (- n 1))))

(define (merge xs ys)
  (cond
    ((null? xs) ys)
    ((null? ys) xs)
    ((<= (car xs) (car ys))
     (cons (car xs) (merge (cdr xs) ys)))
    (else
     (cons (car ys) (merge xs (cdr ys))))))

(define (mergesort lst)
  (let ((n (length lst)))
    (if (<= n 1) lst
        (let ((mid (floor (/ n 2))))
          (merge (mergesort (take lst mid))
                 (mergesort (drop lst mid)))))))

(define data '(38 27 43 3 9 82 10 1 45 17 6 99 23 55 12 8))
(display "Input:  ") (display data) (newline)
(display "Sorted: ") (display (mergesort data)) (newline)
(newline)

;; Sort the same list twice — hash-consing means the result is eq?
(define sorted1 (mergesort data))
(define sorted2 (mergesort data))
(display "Sort same data twice:") (newline)
(display "  (eq? sorted1 sorted2) = ") (display (eq? sorted1 sorted2)) (newline)
(display "  This is #t -- the sorted list was hash-consed!") (newline)
(newline)

;; Larger random-ish list via a simple LCG
(define (lcg-list seed n)
  (let loop ((i 0) (s seed) (acc '()))
    (if (= i n) (reverse acc)
        (let ((next (modulo (+ (* s 1103515245) 12345) 2147483648)))
          (loop (+ i 1) next (cons (modulo next 1000) acc))))))

(define big-data (lcg-list 42 50))
(display "LCG list (50 elements): ") (display big-data) (newline)
(display "Sorted: ") (display (mergesort big-data)) (newline)
(display "Heap size: ") (display (heap-size)) (newline)
