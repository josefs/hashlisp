;;; ==========================================================================
;;; let-insertion.lsp — DAG serialization via let-insertion
;;; ==========================================================================
;;; A symbolic arithmetic DSL where hash-consing creates structural sharing
;;; in the AST. Let-insertion converts the DAG back into a readable `let*`
;;; form, naming each shared subexpression exactly once.
;;;
;;; The key insight: differentiation via the product rule naturally creates
;;; sharing — d/dx(f·g) = f·g' + f'·g reuses both f and g. With hash-
;;; consing, repeated subexpressions are the same pointer. Let-insertion
;;; makes this sharing explicit and serializable.

(display "=== Symbolic Differentiation DSL with Let-Insertion ===") (newline)
(newline)

;;; ── Utilities ────────────────────────────────────────────────────

(define (assq key alist)
  (cond ((null? alist) #f)
        ((eq? key (caar alist)) (car alist))
        (else (assq key (cdr alist)))))

(define (memq x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (else (memq x (cdr lst)))))

;;; ── Smart Constructors ───────────────────────────────────────────
;;; Simplify trivial cases; hash-consing deduplicates the rest.

(define (make-add a b)
  (cond ((and (number? a) (number? b)) (+ a b))
        ((eq? a 0) b)
        ((eq? b 0) a)
        (else (list '+ a b))))

(define (make-mul a b)
  (cond ((and (number? a) (number? b)) (* a b))
        ((eq? a 0) 0)
        ((eq? b 0) 0)
        ((eq? a 1) b)
        ((eq? b 1) a)
        (else (list '* a b))))

;;; ── Symbolic Differentiation (memoized) ──────────────────────────
;;; define-memo caches on (expr, var) — both hash-consed — so shared
;;; AST nodes are differentiated at most once.

(define-memo (diff expr var)
  (cond
    ((number? expr) 0)
    ((symbol? expr) (if (eq? expr var) 1 0))
    ((eq? (car expr) '+)
     (make-add (diff (cadr expr) var)
               (diff (caddr expr) var)))
    ((eq? (car expr) '*)
     (make-add (make-mul (cadr expr) (diff (caddr expr) var))
               (make-mul (diff (cadr expr) var) (caddr expr))))
    (else (error "diff: unknown form"))))

;;; ── Tree Size vs DAG Size ────────────────────────────────────────
;;; tree-size follows every pointer (exponential on shared DAGs).
;;; dag-size counts each unique node once (linear in the DAG).

(define-memo (tree-size expr)
  (if (pair? expr)
      (+ 1 (tree-size (cadr expr)) (tree-size (caddr expr)))
      1))

(define *dag-visited* '())

(define (dag-size expr)
  (set! *dag-visited* '())
  (dag-size-walk expr))

(define (dag-size-walk expr)
  (cond
    ((memq expr *dag-visited*) 0)
    ((pair? expr)
     (set! *dag-visited* (cons expr *dag-visited*))
     (+ 1 (dag-size-walk (cadr expr)) (dag-size-walk (caddr expr))))
    (else
     (set! *dag-visited* (cons expr *dag-visited*))
     1)))

;;; ── Find Shared Subexpressions ───────────────────────────────────
;;; Walk the DAG once. Nodes seen more than once are "shared".

(define *visited* '())
(define *shared* '())

(define (find-shared! expr)
  (when (pair? expr)
    (cond
      ((memq expr *visited*)
       (unless (memq expr *shared*)
         (set! *shared* (cons expr *shared*))))
      (else
       (set! *visited* (cons expr *visited*))
       (find-shared! (cadr expr))
       (find-shared! (caddr expr))))))

;;; ── Let-Insertion ────────────────────────────────────────────────
;;; Convert a hash-consed DAG to a let*-expression with bindings for
;;; every shared subexpression. Post-order traversal ensures bindings
;;; appear before uses.

(define *names* '())
(define *bindings* '())

(define (let-insert expr)
  ;; Phase 1: identify shared nodes
  (set! *visited* '())
  (set! *shared* '())
  (find-shared! expr)
  ;; Phase 2: walk and emit let-bindings
  (set! *names* '())
  (set! *bindings* '())
  (let ((body (li-walk expr)))
    (if (null? *bindings*)
        body
        (list 'let* (reverse *bindings*) body))))

(define (li-walk expr)
  (cond
    ((not (pair? expr)) expr)
    (else
     (let ((named (assq expr *names*)))
       (if named
           ;; Already bound → return the variable
           (cdr named)
           ;; First visit → recurse into operands
           (let* ((left  (li-walk (cadr expr)))
                  (right (li-walk (caddr expr)))
                  (form  (list (car expr) left right)))
             (if (memq expr *shared*)
                 ;; Shared → bind to a fresh variable
                 (let ((name (gensym)))
                   (set! *names* (cons (cons expr name) *names*))
                   (set! *bindings* (cons (list name form) *bindings*))
                   name)
                 ;; Unique → inline
                 form)))))))

;;; ── Pretty-printer for let-inserted forms ────────────────────────

(define (pp expr)
  (cond
    ((and (pair? expr) (eq? (car expr) 'let*))
     (display "(let* (")
     (pp-bindings (cadr expr) #t)
     (display ")")
     (newline)
     (display "  ")
     (write (caddr expr))
     (display ")"))
    (else (write expr))))

(define (pp-bindings bindings first?)
  (when (pair? bindings)
    (unless first? (display "       "))
    (display "(")
    (write (caar bindings))
    (display " ")
    (write (cadr (car bindings)))
    (display ")")
    (when (pair? (cdr bindings)) (newline))
    (pp-bindings (cdr bindings) #f)))

;;; ── DSL Evaluator ────────────────────────────────────────────────

(define (eval-expr expr env)
  (cond
    ((number? expr) expr)
    ((symbol? expr)
     (let ((entry (assq expr env)))
       (if entry (cdr entry) (error "eval-expr: unbound"))))
    ((eq? (car expr) '+)
     (+ (eval-expr (cadr expr) env) (eval-expr (caddr expr) env)))
    ((eq? (car expr) '*)
     (* (eval-expr (cadr expr) env) (eval-expr (caddr expr) env)))
    (else (error "eval-expr: unknown"))))

;;; ══════════════════════════════════════════════════════════════════
;;; Demo
;;; ══════════════════════════════════════════════════════════════════

;; Build x^8 via repeated squaring — minimal DAG, maximal tree blowup
(define e1 (make-mul 'x 'x))
(define e2 (make-mul e1 e1))
(define e3 (make-mul e2 e2))

(display "── Building x^8 via repeated squaring ──") (newline)
(display "  e1 = (* x x)      = x^2") (newline)
(display "  e2 = (* e1 e1)    = x^4") (newline)
(display "  e3 = (* e2 e2)    = x^8") (newline)
(display "  Naive print of e3: ") (write e3) (newline)
(newline)

;; Differentiate three times
(define d1 (diff e3 'x))
(define d2 (diff d1 'x))
(define d3 (diff d2 'x))

;; Show sizes
(display "── Expression sizes ──") (newline)
(display "  Expression       tree-size  dag-size") (newline)

(define (show-stats label expr)
  (let ((ts (tree-size expr))
        (ds (dag-size expr)))
    (display "  ") (display label)
    (display ts)
    (display "        ")
    (display ds)
    (newline)))

(show-stats "x^8            " e3)
(show-stats "d/dx(x^8)      " d1)
(show-stats "d^2/dx^2(x^8)  " d2)
(show-stats "d^3/dx^3(x^8)  " d3)
(newline)

;; Show naive vs let-inserted for d/dx
(display "── Naive serialization of d/dx(x^8) ──") (newline)
(display "  ") (write d1) (newline)
(newline)

;; Show let-inserted forms
(display "── Let-inserted forms ──") (newline)
(newline)

(display "x^8:") (newline)
(display "  ") (pp (let-insert e3)) (newline)
(newline)

(display "d/dx(x^8):") (newline)
(display "  ") (pp (let-insert d1)) (newline)
(newline)

(display "d^2/dx^2(x^8):") (newline)
(display "  ") (pp (let-insert d2)) (newline)
(newline)

(display "d^3/dx^3(x^8):") (newline)
(display "  ") (pp (let-insert d3)) (newline)
(newline)

;; Verify: d^k/dx^k(x^8) at x=2 should be 8!/(8-k)! * 2^(8-k)
(display "── Verification at x = 2 ──") (newline)
(define env (list (cons 'x 2)))
(display "  x^8      = ") (display (eval-expr e3 env))
(display "   (expected 256)") (newline)
(display "  d/dx     = ") (display (eval-expr d1 env))
(display "  (expected 1024)") (newline)
(display "  d^2/dx^2 = ") (display (eval-expr d2 env))
(display "  (expected 3584)") (newline)
(display "  d^3/dx^3 = ") (display (eval-expr d3 env))
(display " (expected 10752)") (newline)
(newline)

;; Show eq?-sharing: subexpressions are pointer-identical
(display "── Hash-consing sharing ──") (newline)
(display "  e1 = (* x x) appears in both e2's children: ")
(display (eq? (cadr e2) (caddr e2))) (newline)
(display "  diff(e1) and the (+ x x) inside d1 share nodes: ")
(let ((de1 (diff e1 'x)))
  (display (eq? de1 (list '+ 'x 'x)))) (newline)
(newline)

(display "── Heap ──") (newline)
(display "  Heap size: ") (display (heap-size)) (newline)
(gc)
(display "  After GC:  ") (display (heap-size)) (newline)
