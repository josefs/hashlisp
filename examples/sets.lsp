;;; ==========================================================================
;;; sets.lsp — Functional Set & Map Library (Hash Trie)
;;; ==========================================================================
;;;
;;; Immutable sets and maps backed by hash tries (binary trie on
;;; the bits of hash-of).  Lives on the hash-consed heap, so:
;;;
;;;   - CANONICAL STRUCTURE: same elements → same trie → eq?-identical.
;;;     This works because the trie shape is determined entirely by
;;;     the hash bits, not by insertion order.
;;;   - Shared subtrees are detected in O(1) by eq?, enabling fast
;;;     union/intersection that skip shared regions.
;;;   - set-insert / map-insert on an existing element returns the
;;;     SAME pointer (eq?), enabling cheap no-change detection.
;;;   - Works as define-memo keys: same set → same cache key.
;;;
;;; Complexity: O(W) insert, lookup, delete where W = hash bit width.
;;;             In practice O(log n) for typical sets.
;;;
;;; Usage:  (load "examples/sets.lsp")

;;; ── Tags ─────────────────────────────────────────────────────────
;;; Leaf and collision nodes use gensym tags so they can't collide
;;; with user data or with each other.

(define *set-leaf* (gensym))
(define *set-collision* (gensym))
(define *map-leaf* (gensym))
(define *map-collision* (gensym))

;;; ── Hash Helpers ─────────────────────────────────────────────────
;;; Extract bits from hash by repeated division.  hash-bit gets the
;;; least significant bit; hash-shift divides by 2 (drops the LSB).

(define (hash-bit h) (modulo h 2))
(define (hash-shift h) (truncate (/ h 2)))

(define (shift-hash-by h n)
  (if (<= n 0) h
      (shift-hash-by (truncate (/ h 2)) (- n 1))))

;; Secondary hash for canonical collision ordering
(define (secondary-hash x) (abs (hash-of (cons x x))))

;;; ── Set Node Constructors and Predicates ─────────────────────────
;;; Empty:       '()
;;; Leaf:        (cons *set-leaf* element)
;;; Branch:      (cons left right)     — left=bit0, right=bit1
;;; Collision:   (cons *set-collision* sorted-element-list)

(define (set-leaf x) (cons *set-leaf* x))
(define (set-leaf? t) (and (pair? t) (eq? (car t) *set-leaf*)))
(define (set-leaf-elem t) (cdr t))

(define (set-collision elems) (cons *set-collision* elems))
(define (set-collision? t) (and (pair? t) (eq? (car t) *set-collision*)))
(define (set-collision-elems t) (cdr t))

;;; ── Set Collision Helpers ────────────────────────────────────────
;;; Collision lists are sorted by secondary-hash for canonical order.

(define (set-col-member? x elems)
  (cond ((null? elems) #f)
        ((eq? x (car elems)) #t)
        (else (set-col-member? x (cdr elems)))))

(define (set-col-insert x elems)
  (let ((hx (secondary-hash x)))
    (let loop ((rest elems))
      (cond
        ((null? rest) (list x))
        ((eq? x (car rest)) elems)    ;; already present → same list
        ((< hx (secondary-hash (car rest)))
         (cons x rest))
        (else
         (let ((tail (loop (cdr rest))))
           (if (eq? tail (cdr rest)) elems
               (cons (car rest) tail))))))))

(define (set-col-delete x elems)
  (cond ((null? elems) '())
        ((eq? x (car elems)) (cdr elems))
        (else
         (let ((rest (set-col-delete x (cdr elems))))
           (if (eq? rest (cdr elems)) elems
               (cons (car elems) rest))))))

(define (set-col-from-pair x y)
  (let ((hx (secondary-hash x)) (hy (secondary-hash y)))
    (if (<= hx hy) (list x y) (list y x))))

;;; ── Branch Constructor (with path compression) ──────────────────
;;; After deletion, collapse branches with one empty child when
;;; the other child is a leaf or collision (preserves canonicality).

(define (set-branch l r)
  (cond
    ((and (null? l) (null? r)) '())
    ((and (null? l) (or (set-leaf? r) (set-collision? r))) r)
    ((and (null? r) (or (set-leaf? l) (set-collision? l))) l)
    (else (cons l r))))

;;; ── Trie Split ──────────────────────────────────────────────────
;;; Create minimal trie separating two non-eq? elements.

(define (set-trie-split x hx y hy)
  (let ((dx (hash-bit hx)) (dy (hash-bit hy)))
    (cond
      ;; Both hashes exhausted → hash collision
      ((and (= hx 0) (= hy 0))
       (set-collision (set-col-from-pair x y)))
      ;; Same bit → descend
      ((= dx dy)
       (let ((sub (set-trie-split x (hash-shift hx) y (hash-shift hy))))
         (if (= dx 0) (cons sub '()) (cons '() sub))))
      ;; Different bits → separate
      (else
       (if (= dx 0)
           (cons (set-leaf x) (set-leaf y))
           (cons (set-leaf y) (set-leaf x)))))))

;;; ══════════════════════════════════════════════════════════════════
;;; SET API
;;; ══════════════════════════════════════════════════════════════════

(define set-empty '())
(define (set-empty? s) (null? s))

(define (set-size s)
  (cond
    ((null? s) 0)
    ((set-leaf? s) 1)
    ((set-collision? s) (length (set-collision-elems s)))
    (else (+ (set-size (car s)) (set-size (cdr s))))))

;;; ── Insert ──────────────────────────────────────────────────────

(define (set-insert x s)
  (set-trie-insert x s (abs (hash-of x)) 0))

(define (set-trie-insert x t hx level)
  (cond
    ((null? t) (set-leaf x))
    ((set-leaf? t)
     (let ((y (set-leaf-elem t)))
       (if (eq? x y) t
           (set-trie-split x hx y
                           (shift-hash-by (abs (hash-of y)) level)))))
    ((set-collision? t)
     (let ((new-elems (set-col-insert x (set-collision-elems t))))
       (if (eq? new-elems (set-collision-elems t)) t
           (set-collision new-elems))))
    ;; Branch
    (else
     (if (= (hash-bit hx) 0)
         (let ((new-l (set-trie-insert x (car t) (hash-shift hx) (+ level 1))))
           (if (eq? new-l (car t)) t (cons new-l (cdr t))))
         (let ((new-r (set-trie-insert x (cdr t) (hash-shift hx) (+ level 1))))
           (if (eq? new-r (cdr t)) t (cons (car t) new-r)))))))

;;; ── Member ──────────────────────────────────────────────────────

(define (set-member? x s)
  (set-trie-member? x s (abs (hash-of x))))

(define (set-trie-member? x t hx)
  (cond
    ((null? t) #f)
    ((set-leaf? t) (eq? x (set-leaf-elem t)))
    ((set-collision? t) (set-col-member? x (set-collision-elems t)))
    (else
     (if (= (hash-bit hx) 0)
         (set-trie-member? x (car t) (hash-shift hx))
         (set-trie-member? x (cdr t) (hash-shift hx))))))

;;; ── Delete ──────────────────────────────────────────────────────

(define (set-delete x s)
  (set-trie-delete x s (abs (hash-of x))))

(define (set-trie-delete x t hx)
  (cond
    ((null? t) t)
    ((set-leaf? t)
     (if (eq? x (set-leaf-elem t)) '() t))
    ((set-collision? t)
     (let ((new-elems (set-col-delete x (set-collision-elems t))))
       (cond ((null? new-elems) '())
             ((null? (cdr new-elems)) (set-leaf (car new-elems)))
             (else (set-collision new-elems)))))
    ;; Branch
    (else
     (if (= (hash-bit hx) 0)
         (let ((new-l (set-trie-delete x (car t) (hash-shift hx))))
           (if (eq? new-l (car t)) t (set-branch new-l (cdr t))))
         (let ((new-r (set-trie-delete x (cdr t) (hash-shift hx))))
           (if (eq? new-r (cdr t)) t (set-branch (car t) new-r)))))))

;;; ── Traversal ───────────────────────────────────────────────────

(define (set-fold f init s)
  (cond
    ((null? s) init)
    ((set-leaf? s) (f (set-leaf-elem s) init))
    ((set-collision? s)
     (let loop ((elems (set-collision-elems s)) (acc init))
       (if (null? elems) acc
           (loop (cdr elems) (f (car elems) acc)))))
    (else (set-fold f (set-fold f init (car s)) (cdr s)))))

(define (set->list s)
  (set-fold (lambda (x acc) (cons x acc)) '() s))

(define (list->set lst)
  (let loop ((rest lst) (s set-empty))
    (if (null? rest) s
        (loop (cdr rest) (set-insert (car rest) s)))))

(define (set-for-each f s)
  (cond
    ((null? s) (void))
    ((set-leaf? s) (f (set-leaf-elem s)))
    ((set-collision? s) (for-each f (set-collision-elems s)))
    (else
     (set-for-each f (car s))
     (set-for-each f (cdr s)))))

;;; ── Set-Theoretic Operations ────────────────────────────────────
;;; These recurse structurally on both tries.  The (eq? s1 s2)
;;; check short-circuits on shared subtrees in O(1).
;;;
;;; Level tracking: when recursing into subtrees at level L, any call
;;; to insert/member/delete must shift the hash by L bits so it
;;; aligns with the subtree's internal structure.

(define (set-insert-at x s level)
  (set-trie-insert x s (shift-hash-by (abs (hash-of x)) level) level))

(define (set-member-at? x s level)
  (set-trie-member? x s (shift-hash-by (abs (hash-of x)) level)))

(define (set-delete-at x s level)
  (set-trie-delete x s (shift-hash-by (abs (hash-of x)) level)))

(define (set-union s1 s2)
  (set-trie-union s1 s2 0))

(define (set-trie-union s1 s2 level)
  (cond
    ((null? s1) s2)
    ((null? s2) s1)
    ((eq? s1 s2) s1)
    ((set-leaf? s1) (set-insert-at (set-leaf-elem s1) s2 level))
    ((set-leaf? s2) (set-insert-at (set-leaf-elem s2) s1 level))
    ((set-collision? s1)
     (let loop ((elems (set-collision-elems s1)) (acc s2))
       (if (null? elems) acc
           (loop (cdr elems) (set-insert-at (car elems) acc level)))))
    ((set-collision? s2) (set-trie-union s2 s1 level))
    (else
     (let ((new-l (set-trie-union (car s1) (car s2) (+ level 1)))
           (new-r (set-trie-union (cdr s1) (cdr s2) (+ level 1))))
       (if (and (eq? new-l (car s1)) (eq? new-r (cdr s1))) s1
           (if (and (eq? new-l (car s2)) (eq? new-r (cdr s2))) s2
               (cons new-l new-r)))))))

(define (set-intersection s1 s2)
  (set-trie-intersection s1 s2 0))

(define (set-trie-intersection s1 s2 level)
  (cond
    ((null? s1) '())
    ((null? s2) '())
    ((eq? s1 s2) s1)
    ((set-leaf? s1)
     (if (set-member-at? (set-leaf-elem s1) s2 level) s1 '()))
    ((set-leaf? s2)
     (if (set-member-at? (set-leaf-elem s2) s1 level) s2 '()))
    ((set-collision? s1)
     (let loop ((elems (set-collision-elems s1)) (acc set-empty))
       (if (null? elems) acc
           (loop (cdr elems)
                 (if (set-member-at? (car elems) s2 level)
                     (set-insert-at (car elems) acc level) acc)))))
    ((set-collision? s2)
     (set-trie-intersection s2 s1 level))
    (else
     (let ((new-l (set-trie-intersection (car s1) (car s2) (+ level 1)))
           (new-r (set-trie-intersection (cdr s1) (cdr s2) (+ level 1))))
       (set-branch new-l new-r)))))

(define (set-difference s1 s2)
  (set-trie-difference s1 s2 0))

(define (set-trie-difference s1 s2 level)
  (cond
    ((null? s1) '())
    ((null? s2) s1)
    ((eq? s1 s2) '())
    ((set-leaf? s1)
     (if (set-member-at? (set-leaf-elem s1) s2 level) '() s1))
    ((set-leaf? s2)
     (set-delete-at (set-leaf-elem s2) s1 level))
    ((set-collision? s1)
     (let loop ((elems (set-collision-elems s1)) (acc set-empty))
       (if (null? elems) acc
           (loop (cdr elems)
                 (if (set-member-at? (car elems) s2 level) acc
                     (set-insert-at (car elems) acc level))))))
    ((set-collision? s2)
     (let loop ((elems (set-collision-elems s2)) (acc s1))
       (if (null? elems) acc
           (loop (cdr elems) (set-delete-at (car elems) acc level)))))
    (else
     (let ((new-l (set-trie-difference (car s1) (car s2) (+ level 1)))
           (new-r (set-trie-difference (cdr s1) (cdr s2) (+ level 1))))
       (if (and (eq? new-l (car s1)) (eq? new-r (cdr s1))) s1
           (set-branch new-l new-r))))))

(define (set-subset? s1 s2)
  (set-trie-subset? s1 s2 0))

(define (set-trie-subset? s1 s2 level)
  (cond
    ((null? s1) #t)
    ((null? s2) #f)
    ((eq? s1 s2) #t)
    ((set-leaf? s1) (set-member-at? (set-leaf-elem s1) s2 level))
    ((set-leaf? s2) #f)
    ((set-collision? s1)
     (let loop ((elems (set-collision-elems s1)))
       (if (null? elems) #t
           (and (set-member-at? (car elems) s2 level)
                (loop (cdr elems))))))
    ((set-collision? s2) #f)
    (else
     (and (set-trie-subset? (car s1) (car s2) (+ level 1))
          (set-trie-subset? (cdr s1) (cdr s2) (+ level 1))))))

(define (set-equal? s1 s2)
  (eq? s1 s2))

;;; ══════════════════════════════════════════════════════════════════
;;; MAP API
;;; ══════════════════════════════════════════════════════════════════
;;; Maps store (key . value) pairs in a trie keyed by (hash-of key).
;;; Canonical: same mapping → same trie → eq?-identical.

;;; ── Map Node Constructors ───────────────────────────────────────

(define (map-leaf k v) (cons *map-leaf* (cons k v)))
(define (map-leaf? t) (and (pair? t) (eq? (car t) *map-leaf*)))
(define (map-leaf-key t) (cadr t))
(define (map-leaf-value t) (cddr t))

(define (map-collision entries) (cons *map-collision* entries))
(define (map-collision? t) (and (pair? t) (eq? (car t) *map-collision*)))
(define (map-collision-entries t) (cdr t))

;;; ── Map Collision Helpers ───────────────────────────────────────

(define (map-col-ref k entries)
  (cond ((null? entries) #f)
        ((eq? k (caar entries)) (car entries))
        (else (map-col-ref k (cdr entries)))))

(define (map-col-insert k v entries)
  (let ((hk (secondary-hash k)))
    (let loop ((rest entries))
      (cond
        ((null? rest) (list (cons k v)))
        ((eq? k (caar rest))
         (if (eq? v (cdar rest)) entries
             (cons (cons k v) (cdr rest))))
        ((< hk (secondary-hash (caar rest)))
         (cons (cons k v) rest))
        (else
         (let ((tail (loop (cdr rest))))
           (if (eq? tail (cdr rest)) entries
               (cons (car rest) tail))))))))

(define (map-col-delete k entries)
  (cond ((null? entries) '())
        ((eq? k (caar entries)) (cdr entries))
        (else
         (let ((rest (map-col-delete k (cdr entries))))
           (if (eq? rest (cdr entries)) entries
               (cons (car entries) rest))))))

(define (map-col-from-pair e1 e2)
  (let ((h1 (secondary-hash (car e1)))
        (h2 (secondary-hash (car e2))))
    (if (<= h1 h2) (list e1 e2) (list e2 e1))))

;;; ── Map Branch Constructor ──────────────────────────────────────

(define (map-branch l r)
  (cond
    ((and (null? l) (null? r)) '())
    ((and (null? l) (or (map-leaf? r) (map-collision? r))) r)
    ((and (null? r) (or (map-leaf? l) (map-collision? l))) l)
    (else (cons l r))))

;;; ── Map Split ───────────────────────────────────────────────────

(define (map-trie-split e1 h1 e2 h2)
  (let ((d1 (hash-bit h1)) (d2 (hash-bit h2)))
    (cond
      ((and (= h1 0) (= h2 0))
       (map-collision (map-col-from-pair e1 e2)))
      ((= d1 d2)
       (let ((sub (map-trie-split e1 (hash-shift h1) e2 (hash-shift h2))))
         (if (= d1 0) (cons sub '()) (cons '() sub))))
      (else
       (let ((l1 (map-leaf (car e1) (cdr e1)))
             (l2 (map-leaf (car e2) (cdr e2))))
         (if (= d1 0) (cons l1 l2) (cons l2 l1)))))))

;;; ── Map Core ────────────────────────────────────────────────────

(define map-empty '())
(define (map-empty? m) (null? m))

(define (map-size m)
  (cond
    ((null? m) 0)
    ((map-leaf? m) 1)
    ((map-collision? m) (length (map-collision-entries m)))
    (else (+ (map-size (car m)) (map-size (cdr m))))))

(define (map-insert k v m)
  (map-trie-insert k v m (abs (hash-of k)) 0))

(define (map-trie-insert k v t hk level)
  (let ((entry (cons k v)))
    (cond
      ((null? t) (map-leaf k v))
      ((map-leaf? t)
       (let ((mk (map-leaf-key t)))
         (if (eq? k mk)
             (if (eq? v (map-leaf-value t)) t
                 (map-leaf k v))
             (map-trie-split entry hk
                             (cons mk (map-leaf-value t))
                             (shift-hash-by (abs (hash-of mk)) level)))))
      ((map-collision? t)
       (let ((new-entries (map-col-insert k v (map-collision-entries t))))
         (if (eq? new-entries (map-collision-entries t)) t
             (map-collision new-entries))))
      ;; Branch
      (else
       (if (= (hash-bit hk) 0)
           (let ((new-l (map-trie-insert k v (car t) (hash-shift hk) (+ level 1))))
             (if (eq? new-l (car t)) t (cons new-l (cdr t))))
           (let ((new-r (map-trie-insert k v (cdr t) (hash-shift hk) (+ level 1))))
             (if (eq? new-r (cdr t)) t (cons (car t) new-r))))))))

(define (map-ref k m)
  (map-trie-ref k m (abs (hash-of k))))

(define (map-trie-ref k t hk)
  (cond
    ((null? t) #f)
    ((map-leaf? t)
     (if (eq? k (map-leaf-key t))
         (cons (map-leaf-key t) (map-leaf-value t))
         #f))
    ((map-collision? t) (map-col-ref k (map-collision-entries t)))
    (else
     (if (= (hash-bit hk) 0)
         (map-trie-ref k (car t) (hash-shift hk))
         (map-trie-ref k (cdr t) (hash-shift hk))))))

(define (map-get k m default)
  (let ((entry (map-ref k m)))
    (if entry (cdr entry) default)))

(define (map-delete k m)
  (map-trie-delete k m (abs (hash-of k))))

(define (map-trie-delete k t hk)
  (cond
    ((null? t) t)
    ((map-leaf? t)
     (if (eq? k (map-leaf-key t)) '() t))
    ((map-collision? t)
     (let ((new-entries (map-col-delete k (map-collision-entries t))))
       (cond ((null? new-entries) '())
             ((null? (cdr new-entries))
              (map-leaf (caar new-entries) (cdar new-entries)))
             (else (map-collision new-entries)))))
    ;; Branch
    (else
     (if (= (hash-bit hk) 0)
         (let ((new-l (map-trie-delete k (car t) (hash-shift hk))))
           (if (eq? new-l (car t)) t (map-branch new-l (cdr t))))
         (let ((new-r (map-trie-delete k (cdr t) (hash-shift hk))))
           (if (eq? new-r (cdr t)) t (map-branch (car t) new-r)))))))

;;; ── Map Traversal ───────────────────────────────────────────────

(define (map-fold f init m)
  (cond
    ((null? m) init)
    ((map-leaf? m) (f (map-leaf-key m) (map-leaf-value m) init))
    ((map-collision? m)
     (let loop ((entries (map-collision-entries m)) (acc init))
       (if (null? entries) acc
           (loop (cdr entries) (f (caar entries) (cdar entries) acc)))))
    (else (map-fold f (map-fold f init (car m)) (cdr m)))))

(define (map->list m)
  (map-fold (lambda (k v acc) (cons (cons k v) acc)) '() m))

(define (map-keys m)
  (map-fold (lambda (k v acc) (cons k acc)) '() m))

(define (alist->map alist)
  (let loop ((rest alist) (m map-empty))
    (if (null? rest) m
        (loop (cdr rest) (map-insert (caar rest) (cdar rest) m)))))

;;; ── Level-Aware Map Helpers ─────────────────────────────────────

(define (map-insert-at k v m level)
  (map-trie-insert k v m (shift-hash-by (abs (hash-of k)) level) level))

(define (map-ref-at k m level)
  (map-trie-ref k m (shift-hash-by (abs (hash-of k)) level)))

(define (map-delete-at k m level)
  (map-trie-delete k m (shift-hash-by (abs (hash-of k)) level)))

;;; Insert with combine: if key exists, call (f old-val new-val)
(define (map-insert-with f k v m)
  (map-trie-insert-with f k v m (abs (hash-of k)) 0))

(define (map-trie-insert-with f k v t hk level)
  (cond
    ((null? t) (map-leaf k v))
    ((map-leaf? t)
     (let ((mk (map-leaf-key t)))
       (if (eq? k mk)
           (let ((combined (f (map-leaf-value t) v)))
             (if (eq? combined (map-leaf-value t)) t
                 (map-leaf k combined)))
           (map-trie-split (cons k v) hk
                           (cons mk (map-leaf-value t))
                           (shift-hash-by (abs (hash-of mk)) level)))))
    ((map-collision? t)
     (let ((existing (map-col-ref k (map-collision-entries t))))
       (if existing
           (let ((combined (f (cdr existing) v)))
             (let ((new-entries (map-col-insert k combined
                                  (map-collision-entries t))))
               (if (eq? new-entries (map-collision-entries t)) t
                   (map-collision new-entries))))
           (map-collision (map-col-insert k v (map-collision-entries t))))))
    (else
     (if (= (hash-bit hk) 0)
         (let ((new-l (map-trie-insert-with f k v (car t)
                         (hash-shift hk) (+ level 1))))
           (if (eq? new-l (car t)) t (cons new-l (cdr t))))
         (let ((new-r (map-trie-insert-with f k v (cdr t)
                         (hash-shift hk) (+ level 1))))
           (if (eq? new-r (cdr t)) t (cons (car t) new-r)))))))

(define (map-insert-with-at f k v m level)
  (map-trie-insert-with f k v m (shift-hash-by (abs (hash-of k)) level) level))

;;; ── Map Union ───────────────────────────────────────────────────
;;; (map-union f m1 m2): merge two maps.
;;; For keys in both maps, call (f val1 val2) to combine values.

(define (map-union f m1 m2)
  (map-trie-union f m1 m2 0))

(define (map-trie-union f m1 m2 level)
  (cond
    ((null? m1) m2)
    ((null? m2) m1)
    ((eq? m1 m2) m1)
    ((map-leaf? m1)
     (let ((entry (map-ref-at (map-leaf-key m1) m2 level)))
       (if entry
           (map-insert-with-at f (map-leaf-key m1) (map-leaf-value m1)
                               m2 level)
           (map-insert-at (map-leaf-key m1) (map-leaf-value m1) m2 level))))
    ((map-leaf? m2)
     (let ((entry (map-ref-at (map-leaf-key m2) m1 level)))
       (if entry
           (map-insert-with-at
             (lambda (v1 v2) (f v1 (map-leaf-value m2)))
             (map-leaf-key m2) (map-leaf-value m2) m1 level)
           (map-insert-at (map-leaf-key m2) (map-leaf-value m2) m1 level))))
    ((map-collision? m1)
     (let loop ((entries (map-collision-entries m1)) (acc m2))
       (if (null? entries) acc
           (let ((e (car entries)))
             (loop (cdr entries)
                   (map-insert-with-at f (car e) (cdr e) acc level))))))
    ((map-collision? m2)
     (map-trie-union (lambda (v1 v2) (f v2 v1)) m2 m1 level))
    (else
     (let ((new-l (map-trie-union f (car m1) (car m2) (+ level 1)))
           (new-r (map-trie-union f (cdr m1) (cdr m2) (+ level 1))))
       (if (and (eq? new-l (car m1)) (eq? new-r (cdr m1))) m1
           (if (and (eq? new-l (car m2)) (eq? new-r (cdr m2))) m2
               (cons new-l new-r)))))))

;;; ── Map Intersection ────────────────────────────────────────────
;;; (map-intersection f m1 m2): keep only keys in both maps.
;;; Values combined with (f val1 val2).

(define (map-intersection f m1 m2)
  (map-trie-intersection f m1 m2 0))

(define (map-trie-intersection f m1 m2 level)
  (cond
    ((null? m1) '())
    ((null? m2) '())
    ((eq? m1 m2) m1)
    ((map-leaf? m1)
     (let ((entry (map-ref-at (map-leaf-key m1) m2 level)))
       (if entry
           (let ((combined (f (map-leaf-value m1) (cdr entry))))
             (map-leaf (map-leaf-key m1) combined))
           '())))
    ((map-leaf? m2)
     (let ((entry (map-ref-at (map-leaf-key m2) m1 level)))
       (if entry
           (let ((combined (f (cdr entry) (map-leaf-value m2))))
             (map-leaf (map-leaf-key m2) combined))
           '())))
    ((map-collision? m1)
     (let loop ((entries (map-collision-entries m1)) (acc map-empty))
       (if (null? entries) acc
           (let* ((e (car entries))
                  (entry2 (map-ref-at (car e) m2 level)))
             (loop (cdr entries)
                   (if entry2
                       (map-insert-at (car e) (f (cdr e) (cdr entry2))
                                      acc level)
                       acc))))))
    ((map-collision? m2)
     (map-trie-intersection (lambda (v1 v2) (f v2 v1)) m2 m1 level))
    (else
     (let ((new-l (map-trie-intersection f (car m1) (car m2) (+ level 1)))
           (new-r (map-trie-intersection f (cdr m1) (cdr m2) (+ level 1))))
       (map-branch new-l new-r)))))

;;; ── Map Keys as Set ─────────────────────────────────────────────
;;; (map-keys->set m): return a set of all keys in the map.

(define (map-keys->set m)
  (map-fold (lambda (k v acc) (set-insert k acc)) set-empty m))

;;; ── Map Restrict ────────────────────────────────────────────────
;;; (map-restrict m s): keep only entries whose key is in set s.

(define (map-restrict m s)
  (map-trie-restrict m s 0))

(define (map-trie-restrict m s level)
  (cond
    ((null? m) '())
    ((null? s) '())
    ((map-leaf? m)
     (if (set-member-at? (map-leaf-key m) s level) m '()))
    ((set-leaf? s)
     (let ((entry (map-ref-at (set-leaf-elem s) m level)))
       (if entry (map-leaf (car entry) (cdr entry)) '())))
    ((map-collision? m)
     (let loop ((entries (map-collision-entries m)) (acc map-empty))
       (if (null? entries) acc
           (loop (cdr entries)
                 (if (set-member-at? (caar entries) s level)
                     (map-insert-at (caar entries) (cdar entries) acc level)
                     acc)))))
    ((set-collision? s)
     (let loop ((elems (set-collision-elems s)) (acc map-empty))
       (if (null? elems) acc
           (let ((entry (map-ref-at (car elems) m level)))
             (loop (cdr elems)
                   (if entry
                       (map-insert-at (car entry) (cdr entry) acc level)
                       acc))))))
    (else
     (let ((new-l (map-trie-restrict (car m) (car s) (+ level 1)))
           (new-r (map-trie-restrict (cdr m) (cdr s) (+ level 1))))
       (if (and (eq? new-l (car m)) (eq? new-r (cdr m))) m
           (map-branch new-l new-r))))))
