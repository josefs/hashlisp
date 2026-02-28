;;; ==========================================================================
;;; kanren.lsp — miniKanren Logic Programming Library
;;; ==========================================================================
;;;
;;; A miniKanren implementation exploiting Hashlisp's hash-consed heap:
;;;
;;;   - Logic variables are hash-consed: eq? gives O(1) identity check
;;;   - Unification short-circuits on eq?: structurally identical terms
;;;     are pointer-equal, so unifying two large identical trees is O(1)
;;;   - Reified answers share structure automatically
;;;
;;; Usage:  (load "examples/kanren.lsp")

;;; ── Logic Variables ──────────────────────────────────────────────
;;; Variables use a gensym tag so they can't collide with user data.

(define *var-tag* (gensym))

(define (var c) (cons *var-tag* c))
(define (var? x) (and (pair? x) (eq? (car x) *var-tag*)))
(define (var-idx x) (cdr x))

;;; ── State ────────────────────────────────────────────────────────
;;; A state is (subst . counter) where:
;;;   subst   = association list ((var . term) ...)
;;;   counter = next fresh variable index

(define empty-state (cons '() 0))

;;; ── Substitution ─────────────────────────────────────────────────

;; Lookup by eq? — free O(1) comparison with hash-consed variables
(define (assq-var v alist)
  (cond ((null? alist) #f)
        ((eq? v (caar alist)) (car alist))
        (else (assq-var v (cdr alist)))))

;; Walk: chase variable bindings to their final value
(define (walk u s)
  (if (var? u)
      (let ((pr (assq-var u s)))
        (if pr (walk (cdr pr) s) u))
      u))

;; Deep walk: recursively resolve all variables in a term
(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)))
      (else v))))

;; Extend substitution
(define (ext-s x v s) (cons (cons x v) s))

;;; ── Unification ──────────────────────────────────────────────────
;;; The eq? check is the hash-consing win: when two walked terms
;;; are structurally identical, they share the same heap node.

(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
      ((eq? u v) s)                    ;; ← O(1) with hash-consing!
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (if s (unify (cdr u) (cdr v) s) #f)))
      (else #f))))

;;; ── Streams ──────────────────────────────────────────────────────
;;; stream = '()                      empty
;;;        | (state . stream)         mature
;;;        | (*s* . thunk)            suspended (for interleaving)

(define *sus-tag* (gensym))

(define (suspend thunk) (cons *sus-tag* thunk))
(define (suspended? $) (and (pair? $) (eq? (car $) *sus-tag*)))

(define (pull $)
  (if (suspended? $)
      (pull ((cdr $)))
      $))

;; Interleaving merge: swaps on suspension for fairness
(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((suspended? $1)
     (suspend (lambda () (mplus $2 ((cdr $1))))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

;; Flatmap a goal over a stream
(define (bind $ g)
  (cond
    ((null? $) '())
    ((suspended? $)
     (suspend (lambda () (bind ((cdr $)) g))))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

;;; ── Goals ────────────────────────────────────────────────────────
;;; A goal is: state → stream

;; Unification goal
(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (cons (cons s (cdr s/c)) '()) '()))))

;; Introduce a fresh logic variable
(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ c 1))))))

;; Disjunction: try both goals
(define (disj g1 g2)
  (lambda (s/c)
    (mplus (g1 s/c) (g2 s/c))))

;; Conjunction: sequence two goals
(define (conj g1 g2)
  (lambda (s/c)
    (bind (g1 s/c) g2)))

;; Suspend a goal for fair interleaving
(define (zzz g)
  (lambda (s/c)
    (suspend (lambda () (g s/c)))))

;; Trivial goals
(define succeed (lambda (s/c) (cons s/c '())))
(define fail (lambda (s/c) '()))

;;; ── Convenience ──────────────────────────────────────────────────

;; fresh with 1–4 variables
(define (fresh/1 f)
  (call/fresh (lambda (a) (f a))))

(define (fresh/2 f)
  (call/fresh (lambda (a)
    (call/fresh (lambda (b) (f a b))))))

(define (fresh/3 f)
  (call/fresh (lambda (a)
    (call/fresh (lambda (b)
      (call/fresh (lambda (c) (f a b c))))))))

(define (fresh/4 f)
  (call/fresh (lambda (a)
    (call/fresh (lambda (b)
      (call/fresh (lambda (c)
        (call/fresh (lambda (d) (f a b c d))))))))))

;; conde: disjunction of goals, each branch suspended
(define (conde2 g1 g2)
  (disj (zzz g1) (zzz g2)))

(define (conde3 g1 g2 g3)
  (disj (zzz g1) (disj (zzz g2) (zzz g3))))

;; Conjunction of a list of goals
(define (conj* goals)
  (if (null? (cdr goals))
      (car goals)
      (conj (car goals) (conj* (cdr goals)))))

;;; ── Take ─────────────────────────────────────────────────────────

(define (take n $)
  (if (= n 0) '()
      (let (($ (pull $)))
        (if (null? $) '()
            (cons (car $) (take (- n 1) (cdr $)))))))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '()
        (cons (car $) (take-all (cdr $))))))

;;; ── Reification ──────────────────────────────────────────────────
;;; Walk the answer fully, then replace remaining logic variables
;;; with readable names: _0, _1, _2, ...

(define *reify-names*
  '(_0 _1 _2 _3 _4 _5 _6 _7 _8 _9
    _10 _11 _12 _13 _14 _15 _16 _17 _18 _19))

(define (list-ref lst n)
  (if (= n 0) (car lst) (list-ref (cdr lst) (- n 1))))

(define (reify-name n)
  (if (< n 20) (list-ref *reify-names* n) n))

;; Build reification substitution: assign _0, _1, ... to free vars
(define (reify-s v rs)
  (let ((v (walk v rs)))
    (cond
      ((var? v) (ext-s v (reify-name (length rs)) rs))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) rs)))
      (else rs))))

(define (reify v s)
  (let* ((v (walk* v s))
         (rs (reify-s v '())))
    (walk* v rs)))

;;; ── Run Interface ────────────────────────────────────────────────
;;; run/run* take a function (lambda (q) goal) where q is the
;;; query variable.  Returns a list of reified answers.

(define (run n g)
  (let (($ (take n ((call/fresh g) empty-state))))
    (map (lambda (s/c) (reify (var 0) (car s/c))) $)))

(define (run* g)
  (let (($ (take-all ((call/fresh g) empty-state))))
    (map (lambda (s/c) (reify (var 0) (car s/c))) $)))

;;; ── Common Relations ─────────────────────────────────────────────

;; appendo: the classic relational append
(define (appendo l s out)
  (conde2
    (conj (== l '()) (== s out))
    (fresh/3 (lambda (a d res)
      (conj* (list (== l (cons a d))
                   (== out (cons a res))
                   (appendo d s res)))))))

;; membero: relational member
(define (membero x lst)
  (fresh/2 (lambda (h t)
    (conj (== lst (cons h t))
          (conde2 (== x h)
                  (membero x t))))))
