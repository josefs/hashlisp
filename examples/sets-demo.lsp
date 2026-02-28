;;; ==========================================================================
;;; sets-demo.lsp — Functional Set & Map Demonstrations
;;; ==========================================================================

(load "examples/sets.lsp")

;;; ── Demo 1: Basic Set Operations ─────────────────────────────────

(display "--- Demo 1: Basic Set Operations ---") (newline)

(define s1 (set-insert 'a (set-insert 'b (set-insert 'c set-empty))))

(display "  s1 = {a b c},  size = ") (display (set-size s1)) (newline)
(display "  member? 'a:  ") (display (set-member? 'a s1)) (newline)
(display "  member? 'd:  ") (display (set-member? 'd s1)) (newline)

(define s2 (set-delete 'b s1))
(display "  delete 'b → size = ") (display (set-size s2)) (newline)
(display "  member? 'b after delete: ") (display (set-member? 'b s2)) (newline)

(display "  set->list s1: ") (display (set->list s1)) (newline)

(newline)

;;; ── Demo 2: Canonical Structure ─────────────────────────────────
;;; THE key property: same elements in ANY order → eq?-identical set.

(display "--- Demo 2: Canonical Structure (order-independent eq?) ---") (newline)

;; Build {a b c} in three different insertion orders
(define order-abc (set-insert 'a (set-insert 'b (set-insert 'c set-empty))))
(define order-cba (set-insert 'c (set-insert 'b (set-insert 'a set-empty))))
(define order-bac (set-insert 'b (set-insert 'a (set-insert 'c set-empty))))

(display "  abc == cba?  ") (display (eq? order-abc order-cba)) (newline)
(display "  abc == bac?  ") (display (eq? order-abc order-bac)) (newline)
(display "  cba == bac?  ") (display (eq? order-cba order-bac)) (newline)

;; Same for integer sets with more elements
(define nums-1 (list->set '(5 3 1 4 2)))
(define nums-2 (list->set '(1 2 3 4 5)))
(define nums-3 (list->set '(2 4 1 5 3)))
(display "  {5,3,1,4,2} == {1,2,3,4,5}?  ") (display (eq? nums-1 nums-2)) (newline)
(display "  {5,3,1,4,2} == {2,4,1,5,3}?  ") (display (eq? nums-1 nums-3)) (newline)

;; Inserting an existing element returns the SAME set
(define s4 (set-insert 'a s1))
(display "  insert existing eq?:    ") (display (eq? s1 s4)) (newline)

;; Deleting a non-member returns the SAME set
(define s5 (set-delete 'z s1))
(display "  delete non-member eq?:  ") (display (eq? s1 s5)) (newline)

(newline)

;;; ── Demo 3: Structural Sharing ──────────────────────────────────

(display "--- Demo 3: Structural Sharing ---") (newline)

(define big (list->set '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
(display "  big set size: ") (display (set-size big)) (newline)

(define big+ (set-insert 99 big))
(display "  big+ size:    ") (display (set-size big+)) (newline)

;; Union of a set with itself returns the SAME set
(define big-union (set-union big big))
(display "  union self eq?:       ") (display (eq? big big-union)) (newline)

;; Intersection of a set with a superset returns the original
(define big-inter (set-intersection big big+))
(display "  intersect superset eq?: ") (display (eq? big big-inter)) (newline)

(newline)

;;; ── Demo 4: Set Operations ─────────────────────────────────────

(display "--- Demo 4: Union, Intersection, Difference ---") (newline)

(define sa (list->set '(1 2 3 4 5)))
(define sb (list->set '(3 4 5 6 7)))

(display "  A = ") (display (set->list sa)) (newline)
(display "  B = ") (display (set->list sb)) (newline)

(define u (set-union sa sb))
(define i (set-intersection sa sb))
(define d (set-difference sa sb))

(display "  A ∪ B size: ") (display (set-size u))
(display "  elements: ") (display (set->list u)) (newline)

(display "  A ∩ B size: ") (display (set-size i))
(display "  elements: ") (display (set->list i)) (newline)

(display "  A \\ B size: ") (display (set-size d))
(display "  elements: ") (display (set->list d)) (newline)

(display "  A ⊆ A∪B?  ") (display (set-subset? sa u)) (newline)
(display "  A∩B ⊆ A?  ") (display (set-subset? i sa)) (newline)

;; set-equal? is just eq? for canonical tries
(display "  A∪B eq? B∪A? ") (display (eq? (set-union sa sb) (set-union sb sa))) (newline)

(newline)

;;; ── Demo 5: Map Operations ─────────────────────────────────────

(display "--- Demo 5: Map Operations ---") (newline)

(define m1 (map-insert 'x 10 (map-insert 'y 20 (map-insert 'z 30 map-empty))))
(display "  map size:    ") (display (map-size m1)) (newline)
(display "  map-get 'x:  ") (display (map-get 'x m1 'not-found)) (newline)
(display "  map-get 'y:  ") (display (map-get 'y m1 'not-found)) (newline)
(display "  map-get 'w:  ") (display (map-get 'w m1 'not-found)) (newline)

;; Update value
(define m2 (map-insert 'x 99 m1))
(display "  after x→99:  ") (display (map-get 'x m2 'not-found)) (newline)
(display "  original x:  ") (display (map-get 'x m1 'not-found)) (newline)

;; Same value → same map pointer
(define m3 (map-insert 'x 10 m1))
(display "  same value eq?: ") (display (eq? m1 m3)) (newline)

;; Canonical: different insertion order → same map
(define m4 (map-insert 'z 30 (map-insert 'x 10 (map-insert 'y 20 map-empty))))
(display "  different order eq?: ") (display (eq? m1 m4)) (newline)

(display "  map->list:   ") (display (map->list m1)) (newline)

(newline)

;;; ── Demo 6: Convergence Detection ──────────────────────────────

(display "--- Demo 6: Convergence Detection ---") (newline)

(define (fixed-point step m)
  (let ((m2 (step m)))
    (if (eq? m m2)
        (begin (display "    converged at size ") (display (map-size m)) (newline)
               m)
        (begin (display "    step... ") (display (map-size m2)) (newline)
               (fixed-point step m2)))))

(define (clamp-step m)
  (let loop ((i 0) (acc m))
    (if (> i 4) acc
        (let ((old (map-get i acc 0)))
          (loop (+ i 1) (map-insert i (min old 3) acc))))))

(display "  fixed-point iteration:") (newline)
(define init-map
  (map-insert 0 1 (map-insert 1 2 (map-insert 2 3
    (map-insert 3 4 (map-insert 4 5 map-empty))))))
(fixed-point clamp-step init-map)

(newline)

;;; ── Demo 7: Map Union, Intersection, Restrict ──────────────────

(display "--- Demo 7: Map Union, Intersection, Keys→Set, Restrict ---") (newline)

(define ma (map-insert 'a 1 (map-insert 'b 2 (map-insert 'c 3 map-empty))))
(define mb (map-insert 'b 10 (map-insert 'c 20 (map-insert 'd 30 map-empty))))

(display "  ma: ") (display (map->list ma)) (newline)
(display "  mb: ") (display (map->list mb)) (newline)

;; Union with +: shared keys get values added
(define mu (map-union + ma mb))
(display "  union (+):  ") (display (map->list mu)) (newline)
(display "    size: ") (display (map-size mu)) (newline)
(display "    a→") (display (map-get 'a mu 'x))
(display " b→") (display (map-get 'b mu 'x))
(display " c→") (display (map-get 'c mu 'x))
(display " d→") (display (map-get 'd mu 'x)) (newline)

;; Intersection with *: only shared keys, values multiplied
(define mi (map-intersection * ma mb))
(display "  intersect (*): ") (display (map->list mi)) (newline)
(display "    size: ") (display (map-size mi)) (newline)
(display "    b→") (display (map-get 'b mi 'x))
(display " c→") (display (map-get 'c mi 'x)) (newline)

;; Keys as set
(define ka (map-keys->set ma))
(define kb (map-keys->set mb))
(display "  keys(ma): ") (display (set->list ka)) (newline)
(display "  keys(mb): ") (display (set->list kb)) (newline)
(display "  keys intersect: ") (display (set->list (set-intersection ka kb))) (newline)

;; Restrict: keep only keys {a, c}
(define keep (list->set '(a c)))
(define mr (map-restrict ma keep))
(display "  ma restricted to {a,c}: ") (display (map->list mr)) (newline)
(display "    size: ") (display (map-size mr)) (newline)

;; Restrict to full key set → same pointer
(define mr-full (map-restrict ma ka))
(display "  restrict to all keys eq?: ") (display (eq? ma mr-full)) (newline)

;; Union with self → same pointer
(display "  union self eq?: ") (display (eq? ma (map-union + ma ma))) (newline)

(newline)

;;; ── Demo 8: Large Set ──────────────────────────────────────────

(display "--- Demo 8: Large Set ---") (newline)

(define (build-set-up n s)
  (if (= n 0) s (build-set-up (- n 1) (set-insert n s))))

(define (build-set-down n s)
  (if (> n 500) s (build-set-down (+ n 1) (set-insert n s))))

(define large-up (build-set-up 500 set-empty))
(define large-down (build-set-down 1 set-empty))

(display "  500-element set, size: ") (display (set-size large-up)) (newline)

(define (check-all n ok)
  (if (= n 0) ok
      (check-all (- n 1) (and ok (set-member? n large-up)))))

(display "  all 500 members found: ") (display (check-all 500 #t)) (newline)
(display "  non-member 501 found:  ") (display (set-member? 501 large-up)) (newline)

;; Insert existing → same pointer
(define large2 (set-insert 250 large-up))
(display "  insert existing eq?:   ") (display (eq? large-up large2)) (newline)

;; THE big test: 500 elements inserted in two opposite orders → eq?
(display "  500↓ eq? 500↑ (canonical!): ") (display (eq? large-up large-down)) (newline)

(newline)
(display "All demos passed.") (newline)
