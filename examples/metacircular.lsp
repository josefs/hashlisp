;;; ==========================================================================
;;; metacircular.lsp — A metacircular evaluator for Hashlisp, in Hashlisp
;;; ==========================================================================
;;; A nearly-complete evaluator for a Hashlisp subset, written in Hashlisp.
;;;
;;; Supported special forms:
;;;   quote, if, cond, define, lambda, let, let*, letrec, named let,
;;;   begin, and, or, when, unless, quasiquote/unquote/unquote-splicing,
;;;   apply
;;;
;;; Lambda features:
;;;   - multi-body (implicit begin)
;;;   - variadic parameters: (lambda (x y . rest) ...) and (lambda args ...)
;;;   - define shorthand: (define (f params...) body...)
;;;
;;; Tail calls are handled by the host — Hashlisp itself has trampoline-based
;;; TCO, so every tail-position my-eval call is optimized automatically.
;;;
;;; Builtins: +, -, *, /, modulo, =, <, >, <=, >=,
;;;   cons, car, cdr, null?, pair?, list, not, eq?, equal?,
;;;   number?, symbol?, boolean?, procedure?,
;;;   length, append, reverse, map, filter, fold,
;;;   apply, error, display, newline, void, void?
;;;
;;; Provides: my-eval, init-env
;;; Demo:     metacircular-demo.lsp

;; ── Environment: association list ────────────────────────────────

(define (env-lookup key env)
  (cond
    ((null? env) #f)
    ((eq? (caar env) key) (cdar env))
    (else (env-lookup key (cdr env)))))

(define (env-define key val env)
  (cons (cons key val) env))

(define (extend-env params vals env)
  (if (null? params) env
      (extend-env (cdr params) (cdr vals)
                  (env-define (car params) (car vals) env))))

;; Extend with variadic params: (x y . rest) binds x, y, then rest to leftover
(define (extend-env-variadic params vals env)
  (cond
    ((null? params) env)
    ((symbol? params)
     ;; rest parameter — bind all remaining vals as a list
     (env-define params vals env))
    (else
     (extend-env-variadic (cdr params) (cdr vals)
                          (env-define (car params) (car vals) env)))))

;; ── Core evaluator ────────────────────────────────────────────────

(define (my-eval expr env)
  (cond
    ;; Self-evaluating: numbers, booleans, strings, void, nil
    ((number? expr)  expr)
    ((boolean? expr) expr)
    ((string? expr)  expr)
    ((null? expr)    expr)
    ((void? expr)    expr)

    ;; Variable lookup
    ((symbol? expr)
     (let ((val (env-lookup expr env)))
       (if val val
           (error (string-append "my-eval: unbound variable")))))

    ;; Special forms (car is a symbol)
    ((pair? expr)
     (let ((head (car expr)))
       (cond
         ;; ── quote ──
         ((eq? head 'quote)
          (cadr expr))

         ;; ── if ──
         ((eq? head 'if)
          (let ((test-val (my-eval (cadr expr) env)))
            (if test-val
                (my-eval (caddr expr) env)
                (if (null? (cdr (cddr expr)))
                    (void)
                    (my-eval (car (cdr (cddr expr))) env)))))

         ;; ── cond ──
         ((eq? head 'cond)
          (eval-cond (cdr expr) env))

         ;; ── and ──
         ((eq? head 'and)
          (eval-and (cdr expr) env))

         ;; ── or ──
         ((eq? head 'or)
          (eval-or (cdr expr) env))

         ;; ── when ──
         ((eq? head 'when)
          (if (my-eval (cadr expr) env)
              (eval-body (cddr expr) env)
              (void)))

         ;; ── unless ──
         ((eq? head 'unless)
          (if (not (my-eval (cadr expr) env))
              (eval-body (cddr expr) env)
              (void)))

         ;; ── begin ──
         ((eq? head 'begin)
          (eval-body (cdr expr) env))

         ;; ── lambda ──
         ((eq? head 'lambda)
          (let ((params (cadr expr))
                (body   (cddr expr)))
            ;; closure = (closure params body env)
            ;; body is a list of expressions (multi-body)
            (list 'closure params body env)))

         ;; ── define ──
         ((eq? head 'define)
          (eval-define (cdr expr) env))

         ;; ── let ──
         ((eq? head 'let)
          (eval-let (cdr expr) env))

         ;; ── let* ──
         ((eq? head 'let*)
          (eval-let-star (cdr expr) env))

         ;; ── letrec ──
         ((eq? head 'letrec)
          (eval-letrec (cdr expr) env))

         ;; ── quasiquote ──
         ((eq? head 'quasiquote)
          (eval-quasiquote (cadr expr) env))

         ;; ── apply ──
         ((eq? head 'apply)
          (let ((fn   (my-eval (cadr expr) env))
                (args (my-eval (caddr expr) env)))
            (apply-fn fn args)))

         ;; ── Regular application ──
         (else
          (let ((fn   (my-eval (car expr) env))
                (args (eval-args (cdr expr) env)))
            (apply-fn fn args))))))

    (else (error "my-eval: cannot evaluate expression"))))

;; ── cond ─────────────────────────────────────────────────────────

(define (eval-cond clauses env)
  (cond
    ((null? clauses) (void))
    (else
     (let ((clause (car clauses)))
       (cond
         ;; (else body...)
         ((eq? (car clause) 'else)
          (eval-body (cdr clause) env))
         (else
          (let ((test-val (my-eval (car clause) env)))
            (if test-val
                (if (null? (cdr clause))
                    test-val
                    (eval-body (cdr clause) env))
                (eval-cond (cdr clauses) env)))))))))

;; ── and / or ─────────────────────────────────────────────────────

(define (eval-and exprs env)
  (cond
    ((null? exprs) #t)
    ((null? (cdr exprs)) (my-eval (car exprs) env))
    (else
     (let ((val (my-eval (car exprs) env)))
       (if (not val) val
           (eval-and (cdr exprs) env))))))

(define (eval-or exprs env)
  (cond
    ((null? exprs) #f)
    ((null? (cdr exprs)) (my-eval (car exprs) env))
    (else
     (let ((val (my-eval (car exprs) env)))
       (if val val
           (eval-or (cdr exprs) env))))))

;; ── begin / body (multi-expression, last value returned) ─────────

(define (eval-body exprs env)
  (cond
    ((null? exprs) (void))
    ((null? (cdr exprs)) (my-eval (car exprs) env))
    (else
     (begin
       (my-eval (car exprs) env)
       (eval-body (cdr exprs) env)))))

;; ── define ───────────────────────────────────────────────────────

(define (eval-define args env)
  ;; (define x expr)  or  (define (f params...) body...)
  (let ((first (car args))
        (rest  (cdr args)))
    (cond
      ;; (define x expr)
      ((symbol? first)
       (let ((val (my-eval (car rest) env)))
         (env-define first val env)))
      ;; (define (f params...) body...) → (define f (rec-closure f params body env))
      ((pair? first)
       (let ((name   (car first))
             (params (cdr first))
             (body   rest))
         ;; Create rec-closure: (rec-closure name params body env)
         ;; The rec-closure will bind name→self in the call env, enabling recursion.
         (let ((clos (list 'rec-closure name params body env)))
           (env-define name clos env))))
      (else (error "my-eval: bad define syntax")))))

;; ── let / let* / letrec / named let ─────────────────────────────

(define (eval-let args env)
  ;; Check if this is named let: (let name ((var init) ...) body ...)
  (if (symbol? (car args))
      (eval-named-let args env)
      (eval-basic-let args env)))

(define (eval-basic-let args env)
  ;; (let ((var init) ...) body ...)
  (let ((bindings (car args))
        (body     (cdr args)))
    ;; Evaluate all inits in the outer env
    (let ((child-env (eval-let-bindings bindings env env)))
      (eval-body body child-env))))

(define (eval-let-bindings bindings eval-env extend-env-val)
  ;; Evaluate each (var init) binding; init is evaluated in eval-env,
  ;; result is added to extend-env-val
  (if (null? bindings) extend-env-val
      (let ((b (car bindings)))
        (let ((var (car b))
              (val (my-eval (cadr b) eval-env)))
          (eval-let-bindings (cdr bindings) eval-env
                             (env-define var val extend-env-val))))))

(define (eval-named-let args env)
  ;; (let name ((var init) ...) body ...)
  ;; Desugar to: (letrec ((name (lambda (params...) body...))) (name inits...))
  (let ((name     (car args))
        (bindings (cadr args))
        (body     (cddr args)))
    (let ((params (map car bindings))
          (init-exprs (map cadr bindings)))
      ;; Build the letrec + call expression and evaluate it
      (let ((lambda-form (cons 'lambda (cons params body)))
            (call-form   (cons name init-exprs)))
        (let ((letrec-form (list 'letrec
                                 (list (list name lambda-form))
                                 call-form)))
          (my-eval letrec-form env))))))

(define (eval-let-star args env)
  ;; (let* ((var init) ...) body ...)
  (let ((bindings (car args))
        (body     (cdr args)))
    (let ((child-env (eval-let-star-bindings bindings env)))
      (eval-body body child-env))))

(define (eval-let-star-bindings bindings env)
  (if (null? bindings) env
      (let ((b (car bindings)))
        (let ((var (car b))
              (val (my-eval (cadr b) env)))
          (eval-let-star-bindings (cdr bindings)
                                  (env-define var val env))))))

(define (eval-letrec args env)
  ;; (letrec ((var init) ...) body ...)
  ;; For each binding that produces a closure, wrap it as a rec-closure
  ;; so it can refer to itself by name at call time.
  ;; For mutual recursion, all bindings are added to the env that each
  ;; closure captures.
  (let ((bindings (car args))
        (body     (cdr args)))
    (let ((names (map car bindings))
          (init-exprs (map cadr bindings)))
      ;; First pass: evaluate all inits in an env where names are
      ;; bound to void (so lambda bodies can close over the names).
      (let ((pre-env (letrec-pre-bind names env)))
        (let ((vals (map (lambda (init) (my-eval init pre-env)) init-exprs)))
          ;; Convert closures to rec-closures
          (let ((rec-vals (letrec-make-rec names vals)))
            ;; Build the final env with all rec-closure bindings
            (let ((result-env (extend-env names rec-vals env)))
              (eval-body body result-env))))))))

(define (letrec-make-rec names vals)
  ;; Pair up names with vals; convert closure→rec-closure
  (if (null? names) '()
      (let ((name (car names))
            (val  (car vals)))
        (cons (if (and (pair? val) (eq? (car val) 'closure))
                  ;; (closure params body env) → (rec-closure name params body env)
                  (list 'rec-closure name (cadr val) (caddr val)
                        (car (cdr (cddr val))))
                  val)
              (letrec-make-rec (cdr names) (cdr vals))))))

(define (letrec-pre-bind names env)
  (if (null? names) env
      (letrec-pre-bind (cdr names)
                       (env-define (car names) (void) env))))

;; ── quasiquote ───────────────────────────────────────────────────

(define (eval-quasiquote tmpl env)
  (cond
    ((not (pair? tmpl)) tmpl)
    ;; (unquote expr)
    ((eq? (car tmpl) 'unquote)
     (my-eval (cadr tmpl) env))
    ;; Check for (unquote-splicing ...) in car position
    ((and (pair? (car tmpl))
          (eq? (caar tmpl) 'unquote-splicing))
     (append (my-eval (cadr (car tmpl)) env)
             (eval-quasiquote (cdr tmpl) env)))
    (else
     (cons (eval-quasiquote (car tmpl) env)
           (eval-quasiquote (cdr tmpl) env)))))

;; ── Argument evaluation ──────────────────────────────────────────

(define (eval-args exprs env)
  (if (null? exprs) '()
      (cons (my-eval (car exprs) env)
            (eval-args (cdr exprs) env))))

;; ── Application ──────────────────────────────────────────────────

(define (apply-fn fn args)
  (cond
    ;; Primitive (symbol tag)
    ((symbol? fn)
     (call-prim fn args))
    ;; Recursive closure: (rec-closure name params body env)
    ;; On application, binds name→self in the call environment.
    ((and (pair? fn) (eq? (car fn) 'rec-closure))
     (let ((name    (cadr fn))
           (params  (caddr fn))
           (body    (car (cdr (cddr fn))))
           (cenv    (car (cdr (cdr (cddr fn))))))
       (let ((call-env (env-define name fn
                         (extend-env-variadic params args cenv))))
         (eval-body body call-env))))
    ;; Closure: (closure params body env)
    ((and (pair? fn) (eq? (car fn) 'closure))
     (let ((params  (cadr fn))
           (body    (caddr fn))
           (cenv    (car (cdr (cddr fn)))))
       (let ((call-env (extend-env-variadic params args cenv)))
         (eval-body body call-env))))
    (else (error "my-eval: not a function"))))

;; ── Primitives ───────────────────────────────────────────────────

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
       (or (symbol? v)  ;; our prim representation
           (and (pair? v) (eq? (car v) 'closure))
           (and (pair? v) (eq? (car v) 'rec-closure)))))
    ((eq? name 'prim-void)   (void))
    ((eq? name 'prim-void?)  (void? (car args)))

    ;; List operations
    ((eq? name 'prim-length)  (length (car args)))
    ((eq? name 'prim-append)  (apply append args))
    ((eq? name 'prim-reverse) (reverse (car args)))
    ((eq? name 'prim-map)
     ;; (map f lst) — f is a meta closure, needs apply-fn
     (meta-map (car args) (cadr args)))
    ((eq? name 'prim-filter)
     (meta-filter (car args) (cadr args)))
    ((eq? name 'prim-fold)
     (meta-fold (car args) (cadr args) (caddr args)))

    ;; apply
    ((eq? name 'prim-apply)
     (apply-fn (car args) (cadr args)))

    ;; error
    ((eq? name 'prim-error)  (error (car args)))

    ;; display / newline (for debugging in meta-programs)
    ((eq? name 'prim-display) (begin (display (car args)) (void)))
    ((eq? name 'prim-newline) (begin (newline) (void)))

    (else (error (string-append "my-eval: unknown primitive: "
                                (number->string 0))))))

;; Higher-order prims need special handling because the function is a
;; meta-level closure, not a host function.

(define (meta-map fn lst)
  (if (null? lst) '()
      (cons (apply-fn fn (list (car lst)))
            (meta-map fn (cdr lst)))))

(define (meta-filter fn lst)
  (if (null? lst) '()
      (if (apply-fn fn (list (car lst)))
          (cons (car lst) (meta-filter fn (cdr lst)))
          (meta-filter fn (cdr lst)))))

(define (meta-fold fn acc lst)
  (if (null? lst) acc
      (meta-fold fn (apply-fn fn (list (car lst) acc)) (cdr lst))))

;; ── Initial environment with primitives ──────────────────────────

(define init-env
  (list
   ;; Arithmetic
   (cons '+ 'prim-add)
   (cons '- 'prim-sub)
   (cons '* 'prim-mul)
   (cons '/ 'prim-div)
   (cons 'modulo 'prim-mod)

   ;; Comparison
   (cons '= 'prim-eq-num)
   (cons '< 'prim-lt)
   (cons '> 'prim-gt)
   (cons '<= 'prim-le)
   (cons '>= 'prim-ge)

   ;; Pairs & lists
   (cons 'cons 'prim-cons)
   (cons 'car 'prim-car)
   (cons 'cdr 'prim-cdr)
   (cons 'null? 'prim-null?)
   (cons 'pair? 'prim-pair?)
   (cons 'list 'prim-list)

   ;; Boolean & equality
   (cons 'not 'prim-not)
   (cons 'eq? 'prim-eq?)
   (cons 'equal? 'prim-equal?)

   ;; Type predicates
   (cons 'number? 'prim-number?)
   (cons 'symbol? 'prim-symbol?)
   (cons 'boolean? 'prim-boolean?)
   (cons 'procedure? 'prim-procedure?)
   (cons 'void 'prim-void)
   (cons 'void? 'prim-void?)

   ;; List operations
   (cons 'length 'prim-length)
   (cons 'append 'prim-append)
   (cons 'reverse 'prim-reverse)
   (cons 'map 'prim-map)
   (cons 'filter 'prim-filter)
   (cons 'fold 'prim-fold)

   ;; apply
   (cons 'apply 'prim-apply)

   ;; error
   (cons 'error 'prim-error)

   ;; I/O
   (cons 'display 'prim-display)
   (cons 'newline 'prim-newline)

   ;; Boolean constants
   (cons '#t #t)
   (cons '#f #f)))
