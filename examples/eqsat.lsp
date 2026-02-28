;;; ==========================================================================
;;; eqsat.lsp — Equality Saturation Library (Hash-Consed)
;;; ==========================================================================
;;;
;;; Equality saturation finds equivalent forms of an expression by:
;;;   1. Building an e-graph (equivalence graph) over terms
;;;   2. Applying rewrite rules that merge e-classes
;;;   3. Extracting the best (smallest) equivalent term
;;;
;;; Hash-consing synergy:
;;;   - E-nodes are lists (op child-id ...) — hash-consed automatically.
;;;     After canonicalization, structurally identical e-nodes are eq?,
;;;     giving us FREE congruence detection.
;;;   - The hashcons table (e-node → class-id) uses eq? for O(1) lookups.
;;;   - Merged e-classes that produce identical canonical e-nodes are
;;;     detected immediately without explicit congruence closure.
;;;
;;; Usage:  (load "examples/eqsat.lsp")

;;; ── Utility: alist operations with integer keys ──────────────────

(define (alist-ref key alist)
  (cond ((null? alist) #f)
        ((= key (caar alist)) (cdar alist))
        (else (alist-ref key (cdr alist)))))

(define (alist-set key val alist)
  (cond ((null? alist) (list (cons key val)))
        ((= key (caar alist)) (cons (cons key val) (cdr alist)))
        (else (cons (car alist) (alist-set key val (cdr alist))))))

;;; ── Utility: alist operations with eq? keys (for hash-consed nodes) ──

(define (alist-ref-eq key alist)
  (cond ((null? alist) #f)
        ((eq? key (caar alist)) (cdar alist))
        (else (alist-ref-eq key (cdr alist)))))

;;; ── E-graph state ────────────────────────────────────────────────
;;;
;;; The e-graph is stored in global variables (mutated via set!):
;;;   *eg-parent*   — union-find: alist of (id . parent-id)
;;;   *eg-classes*  — alist of (id . list-of-enodes)
;;;   *eg-memo*     — hashcons: alist of (enode . class-id), uses eq?
;;;   *eg-next-id*  — next fresh class ID
;;;   *eg-pending*  — list of class IDs needing rebuild

(define *eg-parent* '())
(define *eg-classes* '())
(define *eg-memo* '())
(define *eg-next-id* 0)
(define *eg-pending* '())

;; Reset the e-graph
(define (eg-reset!)
  (set! *eg-parent* '())
  (set! *eg-classes* '())
  (set! *eg-memo* '())
  (set! *eg-next-id* 0)
  (set! *eg-pending* '()))

;;; ── E-node representation ────────────────────────────────────────
;;;
;;; An e-node is either:
;;;   - A leaf: a number or symbol (self-evaluating, no children)
;;;   - An application: (operator child-class-id ...)
;;;     This is a hash-consed list!
;;;
;;; Because e-nodes are hash-consed, two e-nodes with the same
;;; operator and same (canonical) children are eq? — this gives
;;; us automatic congruence detection.

(define (enode-leaf? x)
  (or (number? x) (symbol? x)))

(define (enode-children enode)
  (if (enode-leaf? enode) '()
      (cdr enode)))

(define (enode-op enode)
  (if (enode-leaf? enode) enode
      (car enode)))

;;; ── Union-Find ───────────────────────────────────────────────────

(define (eg-find id)
  (let ((parent (alist-ref id *eg-parent*)))
    (if (= parent id) id
        (let ((root (eg-find parent)))
          ;; Path compression
          (set! *eg-parent* (alist-set id root *eg-parent*))
          root))))

;;; ── Hashcons lookup ──────────────────────────────────────────────
;;;
;;; Since e-nodes are hash-consed lists, we use eq? for lookup.
;;; This is the key advantage: the heap's hash table IS our
;;; unique table.

(define (eg-lookup enode)
  (alist-ref-eq enode *eg-memo*))

;;; ── Canonicalize an e-node ───────────────────────────────────────
;;;
;;; Replace each child class ID with its canonical representative.
;;; After canonicalization, if two e-nodes are congruent, they will
;;; be structurally identical — and because of hash-consing, they
;;; will be eq?!

(define (eg-canonicalize enode)
  (if (enode-leaf? enode)
      enode
      (cons (car enode) (map eg-find (cdr enode)))))

;;; ── Add a term to the e-graph ────────────────────────────────────
;;;
;;; Recursively adds subterms, building e-nodes bottom-up.
;;; Returns the class ID for the term.

(define (eg-add-term term)
  (if (enode-leaf? term)
      ;; Leaf
      (let ((existing (eg-lookup term)))
        (if existing
            (eg-find existing)
            (eg-make-class term)))
      ;; Application: add children first
      (let* ((child-ids (map eg-add-term (cdr term)))
             (enode (cons (car term) child-ids))
             (existing (eg-lookup enode)))
        (if existing
            (eg-find existing)
            (eg-make-class enode)))))

;; Internal: create a fresh e-class containing one e-node
(define (eg-make-class enode)
  (let ((id *eg-next-id*))
    (set! *eg-next-id* (+ id 1))
    (set! *eg-parent* (cons (cons id id) *eg-parent*))
    (set! *eg-classes* (cons (cons id (list enode)) *eg-classes*))
    (set! *eg-memo* (cons (cons enode id) *eg-memo*))
    id))

;;; ── Merge two e-classes ──────────────────────────────────────────

(define (eg-merge id1 id2)
  (let ((r1 (eg-find id1))
        (r2 (eg-find id2)))
    (if (= r1 r2)
        r1
        (begin
          ;; Union: r1 becomes root
          (set! *eg-parent* (alist-set r2 r1 *eg-parent*))
          ;; Merge enodes from both classes
          (let ((nodes1 (alist-ref r1 *eg-classes*))
                (nodes2 (alist-ref r2 *eg-classes*)))
            (set! *eg-classes*
                  (alist-set r1 (append nodes1 nodes2) *eg-classes*)))
          ;; Schedule for rebuild
          (set! *eg-pending* (cons r1 *eg-pending*))
          r1))))

;;; ── Rebuild: restore the congruence invariant ────────────────────
;;;
;;; After merges, some e-nodes may need re-canonicalization.
;;; If two e-nodes become identical after canonicalization, their
;;; classes must be merged. With hash-consing, this check is free:
;;; after canonicalization, identical e-nodes are eq?.

(define (eg-rebuild!)
  (let loop ()
    (if (null? *eg-pending*)
        'done
        (let ((to-process *eg-pending*))
          (set! *eg-pending* '())
          (for-each eg-repair! to-process)
          ;; Repairs may trigger more merges → keep going
          (loop)))))

(define (eg-repair! class-id)
  (let* ((id (eg-find class-id))
         (enodes (alist-ref id *eg-classes*)))
    (if (not enodes) 'done
        (begin
          ;; Remove old memo entries for this class's enodes
          (for-each
            (lambda (enode)
              (set! *eg-memo* (alist-remove-eq enode *eg-memo*)))
            enodes)
          ;; Re-canonicalize all enodes and re-insert
          (let ((canonical-nodes (map eg-canonicalize enodes)))
            ;; Deduplicate and check for congruences
            (let dedup ((nodes canonical-nodes) (seen '()) (kept '()))
              (if (null? nodes)
                  ;; Update the class with deduplicated enodes
                  (begin
                    (set! *eg-classes* (alist-set id (reverse kept) *eg-classes*))
                    ;; Re-insert into memo
                    (for-each
                      (lambda (enode)
                        (let ((existing (eg-lookup enode)))
                          (if existing
                              ;; Congruence! Another class has this same enode
                              (eg-merge id existing)
                              ;; Fresh entry
                              (set! *eg-memo*
                                    (cons (cons enode id) *eg-memo*)))))
                      (reverse kept)))
                  ;; Check if this canonical enode was already seen
                  (if (memq-eq? (car nodes) seen)
                      (dedup (cdr nodes) seen kept)
                      (dedup (cdr nodes)
                             (cons (car nodes) seen)
                             (cons (car nodes) kept))))))))))

;; Helper: remove an entry by eq? key
(define (alist-remove-eq key alist)
  (cond ((null? alist) '())
        ((eq? key (caar alist)) (cdr alist))
        (else (cons (car alist) (alist-remove-eq key (cdr alist))))))

;; Helper: memq with eq?
(define (memq-eq? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (else (memq-eq? x (cdr lst)))))

;;; ── Pattern matching (e-matching) ────────────────────────────────
;;;
;;; Patterns are S-expressions where (? name) denotes a variable.
;;; E-matching finds all substitutions such that the pattern matches
;;; some e-node in some e-class.
;;;
;;; Returns a list of (class-id . substitution) pairs, where
;;; substitution is an alist of (var-name . class-id).

(define (pattern-var? x)
  (and (pair? x) (eq? (car x) '?)))

(define (pattern-var-name x) (cadr x))

;; Match a pattern against the e-graph.
;; Returns list of (class-id . subst) pairs.
(define (eg-ematch pattern)
  (let ((results '()))
    (for-each
      (lambda (class-entry)
        (let ((class-id (eg-find (car class-entry))))
          (let ((matches (eg-match-in-class pattern class-id)))
            (for-each
              (lambda (subst)
                (set! results (cons (cons class-id subst) results)))
              matches))))
      *eg-classes*)
    results))

;; Match a pattern in a specific class.
;; Returns a list of substitutions (each is an alist).
(define (eg-match-in-class pattern class-id)
  (let ((id (eg-find class-id)))
    (cond
      ;; Pattern variable: matches any class
      ((pattern-var? pattern)
       (list (list (cons (pattern-var-name pattern) id))))
      ;; Leaf pattern: matches if the class contains this leaf
      ((enode-leaf? pattern)
       (let ((enodes (alist-ref id *eg-classes*)))
         (if (and enodes (memq-eq? pattern enodes))
             (list '())  ; match with empty substitution
             '())))
      ;; Application pattern: match against application enodes
      (else
       (let ((pat-op (car pattern))
             (pat-children (cdr pattern))
             (enodes (alist-ref id *eg-classes*)))
         (if (not enodes) '()
             (let ((matching '()))
               (for-each
                 (lambda (enode)
                   (if (and (not (enode-leaf? enode))
                            (eq? (car enode) pat-op)
                            (= (length (cdr enode)) (length pat-children)))
                       (let ((substs (eg-match-children
                                       pat-children (cdr enode) (list '()))))
                         (set! matching (append substs matching)))))
                 enodes)
               matching)))))))

;; Match pattern children against e-node children.
;; Thread substitutions through: each child match produces a refined subst.
(define (eg-match-children pat-kids enode-kids substs)
  (if (null? pat-kids)
      substs
      (let ((new-substs '()))
        (for-each
          (lambda (subst)
            (let ((matches (eg-match-child (car pat-kids) (car enode-kids) subst)))
              (set! new-substs (append matches new-substs))))
          substs)
        (eg-match-children (cdr pat-kids) (cdr enode-kids) new-substs))))

;; Match one pattern child against one e-node child (a class ID)
;; given an existing substitution.
(define (eg-match-child pattern class-id subst)
  (cond
    ;; Pattern variable
    ((pattern-var? pattern)
     (let* ((name (pattern-var-name pattern))
            (bound (assq-sym name subst)))
       (if bound
           ;; Already bound: check consistency
           (if (= (cdr bound) (eg-find class-id))
               (list subst) '())
           ;; Unbound: extend substitution
           (list (cons (cons name (eg-find class-id)) subst)))))
    ;; Leaf pattern
    ((enode-leaf? pattern)
     (let ((id (eg-find class-id))
           (enodes (alist-ref (eg-find class-id) *eg-classes*)))
       (if (and enodes (memq-eq? pattern enodes))
           (list subst) '())))
    ;; Application: recurse into the class
    (else
     (let ((matches (eg-match-in-class pattern (eg-find class-id))))
       (let ((merged '()))
         (for-each
           (lambda (new-subst)
             (let ((combined (merge-substs subst new-subst)))
               (if combined
                   (set! merged (cons combined merged)))))
           matches)
         merged)))))

;; Merge two substitutions; return #f if they conflict
(define (merge-substs s1 s2)
  (let loop ((s s2) (acc s1))
    (if (null? s) acc
        (let* ((pair (car s))
               (existing (assq-sym (car pair) acc)))
          (if existing
              (if (= (cdr existing) (cdr pair))
                  (loop (cdr s) acc)
                  #f)  ; conflict
              (loop (cdr s) (cons pair acc)))))))

;; assq but using eq? on symbol keys
(define (assq-sym key alist)
  (cond ((null? alist) #f)
        ((eq? key (caar alist)) (car alist))
        (else (assq-sym key (cdr alist)))))

;;; ── Rewrite rules ────────────────────────────────────────────────
;;;
;;; A rule is (lhs . rhs) where both are patterns.
;;; Rules are bidirectional in equality saturation: we add the rhs
;;; as equivalent to the lhs's class.
;;;
;;; (make-rule '(+ (? a) 0) '(? a))
;;;   means:  (+ x 0) is equivalent to x

(define (make-rule lhs rhs)
  (cons lhs rhs))

(define (rule-lhs r) (car r))
(define (rule-rhs r) (cdr r))

;;; ── Instantiate a pattern with a substitution ────────────────────
;;;
;;; Build a concrete term from a pattern + substitution,
;;; then add it to the e-graph.

(define (eg-instantiate pattern subst)
  (cond
    ((pattern-var? pattern)
     (let ((binding (assq-sym (pattern-var-name pattern) subst)))
       (if binding (cdr binding)
           (error "unbound pattern variable"))))
    ((enode-leaf? pattern)
     (eg-add-term pattern))
    (else
     (let ((child-ids (map (lambda (p) (eg-instantiate p subst))
                           (cdr pattern))))
       (let* ((enode (cons (car pattern) child-ids))
              (existing (eg-lookup enode)))
         (if existing
             (eg-find existing)
             (eg-make-class enode)))))))

;;; ── Apply one rule to the entire e-graph ─────────────────────────

(define (eg-apply-rule rule)
  (let* ((matches (eg-ematch (rule-lhs rule)))
         (n-merges 0))
    (for-each
      (lambda (match)
        (let ((matched-class (car match))
              (subst (cdr match)))
          (let ((new-class (eg-instantiate (rule-rhs rule) subst)))
            (if (not (= (eg-find matched-class) (eg-find new-class)))
                (begin
                  (eg-merge matched-class new-class)
                  (set! n-merges (+ n-merges 1)))))))
      matches)
    n-merges))

;;; ── Saturate: apply rules until fixpoint or limit ────────────────

(define (eg-saturate! rules max-iters)
  (let loop ((iter 0))
    (if (>= iter max-iters)
        (begin
          (display "  Reached iteration limit: ") (display max-iters) (newline)
          iter)
        (let ((total-merges 0))
          (for-each
            (lambda (rule)
              (let ((n (eg-apply-rule rule)))
                (set! total-merges (+ total-merges n))))
            rules)
          (eg-rebuild!)
          (if (= total-merges 0)
              (begin
                (display "  Saturated at iteration ") (display (+ iter 1)) (newline)
                (+ iter 1))
              (loop (+ iter 1)))))))

;;; ── Extract: find the smallest term in an e-class ────────────────
;;;
;;; Simple cost model: each operator costs 1, leaves cost 1,
;;; total cost is the sum.  Uses memoization to avoid exponential
;;; re-traversal of shared structure.

(define (eg-extract class-id)
  (let ((memo '()))
    ;; Returns (cost . term)
    (define (best id)
      (let* ((id (eg-find id))
             (cached (alist-ref id memo)))
        (if cached cached
            (begin
              ;; Insert sentinel BEFORE recursing to break cycles
              (set! memo (cons (cons id (cons 999999 '???)) memo))
              (let ((enodes (alist-ref id *eg-classes*)))
                (if (not enodes)
                    (cons 999999 '???)
                    (let ((result (best-of-enodes enodes)))
                      (set! memo (alist-set id result memo))
                      result)))))))

    (define (best-of-enodes enodes)
      (let loop ((nodes enodes) (best-cost 999999) (best-term '???))
        (if (null? nodes)
            (cons best-cost best-term)
            (let ((cost-term (enode-cost (car nodes))))
              (if (< (car cost-term) best-cost)
                  (loop (cdr nodes) (car cost-term) (cdr cost-term))
                  (loop (cdr nodes) best-cost best-term))))))

    (define (enode-cost enode)
      (if (enode-leaf? enode)
          (cons 1 enode)
          (let* ((child-results (map (lambda (cid) (best cid))
                                     (enode-children enode)))
                 (child-costs (map car child-results))
                 (child-terms (map cdr child-results))
                 (total (+ 1 (fold + 0 child-costs))))
            (cons total (cons (enode-op enode) child-terms)))))

    (cdr (best class-id))))

;;; ── E-graph statistics ───────────────────────────────────────────

(define (eg-num-classes)
  (let ((seen '()))
    (for-each
      (lambda (entry)
        (let ((root (eg-find (car entry))))
          (if (not (memv root seen))
              (set! seen (cons root seen)))))
      *eg-parent*)
    (length seen)))

(define (eg-num-enodes)
  (let ((total 0))
    (for-each
      (lambda (entry)
        (if (= (eg-find (car entry)) (car entry))
            (set! total (+ total (length (cdr entry))))))
      *eg-classes*)
    total))

(define (memv x lst)
  (cond ((null? lst) #f)
        ((= x (car lst)) #t)
        (else (memv x (cdr lst)))))
