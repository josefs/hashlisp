;;; ==========================================================================
;;; metacircular-memo.lsp — Metacircular evaluator with memoized eval
;;; ==========================================================================
;;; Demonstrates the effect of define-memo on the eval function.
;;; Since ASTs and environments are hash-consed, identical (expr, env)
;;; pairs hit the memo cache — turning exponential recursion linear.
;;;
;;; Unlike the base evaluator, this version uses an explicit trampoline.
;;; This is necessary because define-memo functions don't get tail-call
;;; optimization — the host's memo wrapper must capture the return value
;;; to cache it, so each recursive call consumes a stack frame.
;;; The trampoline keeps the iteration loop flat: eval-inner returns
;;; a tag, the trampoline loop (host-TCO'd) iterates.
;;;
;;; Provides: memoized eval-inner, trampoline-based my-eval
;;; Demo:     metacircular-memo-demo.lsp

(load "examples/metacircular.lsp")

;; ── Trampoline tags ──────────────────────────────────────────────
;; A "result" is either:
;;   (done . value)          — final value
;;   (tail-call expr . env)  — continue evaluating expr in env

(define (make-done val)      (cons 'done val))
(define (make-tail expr env) (cons 'tail-call (cons expr env)))
(define (done? r)            (eq? (car r) 'done))
(define (done-val r)         (cdr r))
(define (tc-expr r)          (cadr r))
(define (tc-env r)           (cddr r))

;; ── Trampoline driver ────────────────────────────────────────────
;; The host's TCO handles the tail-recursive trampoline call,
;; so this loop uses O(1) stack regardless of iteration depth.

(define (trampoline r)
  (if (done? r) (done-val r)
      (trampoline (eval-inner (tc-expr r) (tc-env r)))))

;; Resolve a trampoline result to a value.
(define (resolve-result r)
  (if (done? r) (done-val r)
      (my-eval (tc-expr r) (tc-env r))))

;; ── Override my-eval to use trampoline ───────────────────────────

(define (my-eval expr env)
  (trampoline (eval-inner expr env)))

;; ── Memoized core evaluator (returns trampoline tags) ────────────

(define-memo (eval-inner expr env)
  (cond
    ;; Self-evaluating
    ((number? expr)  (make-done expr))
    ((boolean? expr) (make-done expr))
    ((string? expr)  (make-done expr))
    ((null? expr)    (make-done expr))
    ((void? expr)    (make-done expr))

    ;; Variable lookup
    ((symbol? expr)
     (let ((val (env-lookup expr env)))
       (if val (make-done val)
           (error (string-append "my-eval: unbound variable")))))

    ;; Special forms
    ((pair? expr)
     (let ((head (car expr)))
       (cond
         ((eq? head 'quote)
          (make-done (cadr expr)))

         ((eq? head 'if)
          (let ((test-val (my-eval (cadr expr) env)))
            (if test-val
                (make-tail (caddr expr) env)
                (if (null? (cdr (cddr expr)))
                    (make-done (void))
                    (make-tail (car (cdr (cddr expr))) env)))))

         ((eq? head 'cond)
          (eval-cond (cdr expr) env))

         ((eq? head 'and)
          (eval-and (cdr expr) env))

         ((eq? head 'or)
          (eval-or (cdr expr) env))

         ((eq? head 'when)
          (if (my-eval (cadr expr) env)
              (eval-body (cddr expr) env)
              (make-done (void))))

         ((eq? head 'unless)
          (if (not (my-eval (cadr expr) env))
              (eval-body (cddr expr) env)
              (make-done (void))))

         ((eq? head 'begin)
          (eval-body (cdr expr) env))

         ((eq? head 'lambda)
          (let ((params (cadr expr))
                (body   (cddr expr)))
            (make-done (list 'closure params body env))))

         ((eq? head 'define)
          (eval-define (cdr expr) env))

         ((eq? head 'let)
          (eval-let (cdr expr) env))

         ((eq? head 'let*)
          (eval-let-star (cdr expr) env))

         ((eq? head 'letrec)
          (eval-letrec (cdr expr) env))

         ((eq? head 'quasiquote)
          (make-done (eval-quasiquote (cadr expr) env)))

         ((eq? head 'apply)
          (let ((fn   (my-eval (cadr expr) env))
                (args (my-eval (caddr expr) env)))
            (apply-fn fn args)))

         (else
          (let ((fn   (my-eval (car expr) env))
                (args (eval-args (cdr expr) env)))
            (apply-fn fn args))))))

    (else (error "my-eval: cannot evaluate expression"))))

;; ── Override helpers to return trampoline tags ───────────────────

(define (eval-cond clauses env)
  (cond
    ((null? clauses) (make-done (void)))
    (else
     (let ((clause (car clauses)))
       (cond
         ((eq? (car clause) 'else)
          (eval-body (cdr clause) env))
         (else
          (let ((test-val (my-eval (car clause) env)))
            (if test-val
                (if (null? (cdr clause))
                    (make-done test-val)
                    (eval-body (cdr clause) env))
                (eval-cond (cdr clauses) env)))))))))

(define (eval-and exprs env)
  (cond
    ((null? exprs) (make-done #t))
    ((null? (cdr exprs)) (make-tail (car exprs) env))
    (else
     (let ((val (my-eval (car exprs) env)))
       (if (not val) (make-done val)
           (eval-and (cdr exprs) env))))))

(define (eval-or exprs env)
  (cond
    ((null? exprs) (make-done #f))
    ((null? (cdr exprs)) (make-tail (car exprs) env))
    (else
     (let ((val (my-eval (car exprs) env)))
       (if val (make-done val)
           (eval-or (cdr exprs) env))))))

;; eval-body: evaluates a body sequence, returns tag for the last expr.
(define (eval-body exprs env)
  (cond
    ((null? exprs) (make-done (void)))
    ((null? (cdr exprs)) (make-tail (car exprs) env))
    (else
     (begin
       (my-eval (car exprs) env)
       (eval-body (cdr exprs) env)))))

(define (eval-define args env)
  (let ((first (car args))
        (rest  (cdr args)))
    (cond
      ((symbol? first)
       (let ((val (my-eval (car rest) env)))
         (make-done (env-define first val env))))
      ((pair? first)
       (let ((name   (car first))
             (params (cdr first))
             (body   rest))
         (let ((clos (list 'rec-closure name params body env)))
           (make-done (env-define name clos env)))))
      (else (error "my-eval: bad define syntax")))))

;; Named let needs to return a tag (called from eval-let in tail position).
(define (eval-named-let args env)
  (let ((name     (car args))
        (bindings (cadr args))
        (body     (cddr args)))
    (let ((params (map car bindings))
          (init-exprs (map cadr bindings)))
      (let ((lambda-form (cons 'lambda (cons params body)))
            (call-form   (cons name init-exprs)))
        (let ((letrec-form (list 'letrec
                                 (list (list name lambda-form))
                                 call-form)))
          (eval-inner letrec-form env))))))

;; ── Override apply-fn to return trampoline tags ──────────────────

(define (apply-fn fn args)
  (cond
    ((symbol? fn)
     (make-done (call-prim fn args)))
    ((and (pair? fn) (eq? (car fn) 'rec-closure))
     (let ((name    (cadr fn))
           (params  (caddr fn))
           (body    (car (cdr (cddr fn))))
           (cenv    (car (cdr (cdr (cddr fn))))))
       (let ((call-env (env-define name fn
                         (extend-env-variadic params args cenv))))
         (eval-body body call-env))))
    ((and (pair? fn) (eq? (car fn) 'closure))
     (let ((params  (cadr fn))
           (body    (caddr fn))
           (cenv    (car (cdr (cddr fn)))))
       (let ((call-env (extend-env-variadic params args cenv)))
         (eval-body body call-env))))
    (else (error "my-eval: not a function"))))

;; ── Override call-prim (prim-apply needs to resolve tags) ────────

(define (call-prim name args)
  (cond
    ;; Arithmetic
    ((eq? name 'prim-add)  (fold + 0 args))
    ((eq? name 'prim-sub)
     (if (null? (cdr args))
         (- 0 (car args))
         (- (car args) (fold + 0 (cdr args)))))
    ((eq? name 'prim-mul)  (fold * 1 args))
    ((eq? name 'prim-div)  (/ (car args) (cadr args)))
    ((eq? name 'prim-mod)  (modulo (car args) (cadr args)))

    ;; Comparison
    ((eq? name 'prim-eq-num) (= (car args) (cadr args)))
    ((eq? name 'prim-lt)     (< (car args) (cadr args)))
    ((eq? name 'prim-gt)     (> (car args) (cadr args)))
    ((eq? name 'prim-le)     (<= (car args) (cadr args)))
    ((eq? name 'prim-ge)     (>= (car args) (cadr args)))

    ;; Pairs and lists
    ((eq? name 'prim-cons)   (cons (car args) (cadr args)))
    ((eq? name 'prim-car)    (car (car args)))
    ((eq? name 'prim-cdr)    (cdr (car args)))
    ((eq? name 'prim-null?)  (null? (car args)))
    ((eq? name 'prim-pair?)  (pair? (car args)))
    ((eq? name 'prim-list)   args)

    ;; Boolean
    ((eq? name 'prim-not)    (not (car args)))
    ((eq? name 'prim-eq?)    (eq? (car args) (cadr args)))
    ((eq? name 'prim-equal?) (equal? (car args) (cadr args)))

    ;; Type predicates
    ((eq? name 'prim-number?)    (number? (car args)))
    ((eq? name 'prim-symbol?)    (symbol? (car args)))
    ((eq? name 'prim-boolean?)   (boolean? (car args)))
    ((eq? name 'prim-procedure?)
     (let ((v (car args)))
       (or (symbol? v)
           (and (pair? v) (eq? (car v) 'closure))
           (and (pair? v) (eq? (car v) 'rec-closure)))))
    ((eq? name 'prim-void)   (void))
    ((eq? name 'prim-void?)  (void? (car args)))

    ;; List operations
    ((eq? name 'prim-length)  (length (car args)))
    ((eq? name 'prim-append)  (apply append args))
    ((eq? name 'prim-reverse) (reverse (car args)))
    ((eq? name 'prim-map)     (meta-map (car args) (cadr args)))
    ((eq? name 'prim-filter)  (meta-filter (car args) (cadr args)))
    ((eq? name 'prim-fold)    (meta-fold (car args) (cadr args) (caddr args)))

    ;; apply — must resolve trampoline result since apply-fn returns tags
    ((eq? name 'prim-apply)
     (resolve-result (apply-fn (car args) (cadr args))))

    ;; error
    ((eq? name 'prim-error)  (error (car args)))

    ;; display / newline
    ((eq? name 'prim-display) (begin (display (car args)) (void)))
    ((eq? name 'prim-newline) (begin (newline) (void)))

    (else (error (string-append "my-eval: unknown primitive: "
                                (number->string 0))))))

;; Override meta-map/filter/fold — apply-fn returns tags, resolve them.

(define (meta-map fn lst)
  (if (null? lst) '()
      (cons (resolve-result (apply-fn fn (list (car lst))))
            (meta-map fn (cdr lst)))))

(define (meta-filter fn lst)
  (if (null? lst) '()
      (let ((val (resolve-result (apply-fn fn (list (car lst))))))
        (if val
            (cons (car lst) (meta-filter fn (cdr lst)))
            (meta-filter fn (cdr lst))))))

(define (meta-fold fn acc lst)
  (if (null? lst) acc
      (meta-fold fn
                 (resolve-result (apply-fn fn (list (car lst) acc)))
                 (cdr lst))))
