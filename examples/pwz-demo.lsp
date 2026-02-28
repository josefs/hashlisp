;;; ==========================================================================
;;; pwz-demo.lsp — Parsing with Derivatives Demo (loads pwz.lsp)
;;; ==========================================================================

(load "examples/pwz.lsp")

(display "=== Parsing with Derivatives Demo ===") (newline) (newline)

;;; ── Demo 1: Literal matching ─────────────────────────────────────

(display "── Demo 1: Literal matching ──") (newline)

(define g-abc (lits '(a b c)))
(display "  parse (a b c): ") (display (parse g-abc '(a b c))) (newline)
(display "  parse (a b d): ") (display (parse g-abc '(a b d))) (newline)
(display "  parse (a b):   ") (display (parse g-abc '(a b)))   (newline)
(newline)

;;; ── Demo 2: Alternation ─────────────────────────────────────────

(display "── Demo 2: Alternation ──") (newline)

(define g-ab|cd (alt (lits '(a b)) (lits '(c d))))
(display "  parse (a b): ") (display (parse g-ab|cd '(a b))) (newline)
(display "  parse (c d): ") (display (parse g-ab|cd '(c d))) (newline)
(display "  parse (a c): ") (display (parse g-ab|cd '(a c))) (newline)
(newline)

;;; ── Demo 3: Reduction (semantic actions) ─────────────────────────

(display "── Demo 3: Reduction ──") (newline)

(define g-digit
  (alts (map (lambda (d) (red (lit d) (lambda (x) d)))
             '(0 1 2 3 4 5 6 7 8 9))))

(display "  parse digit (5): ") (display (parse g-digit '(5))) (newline)
(display "  parse digit (a): ") (display (parse g-digit '(a))) (newline)

(define g-two-digit
  (red (seq g-digit g-digit)
       (lambda (p) (+ (* 10 (car p)) (cdr p)))))

(display "  parse 2-digit (4 2): ") (display (parse g-two-digit '(4 2))) (newline)
(newline)

;;; ── Demo 4: Recursive grammar (parenthesized expressions) ───────

(display "── Demo 4: Recursive grammar ──") (newline)

;; S → a | ( S )
(define-grammar! 'S
  (alt (red (lit 'a) (lambda (x) 'a))
       (red (seq (lit 'LP) (seq (ref 'S) (lit 'RP)))
            (lambda (p)
              (car (cdr p))))))

(display "  parse (a):               ") (display (parse (ref 'S) '(a))) (newline)
(display "  parse (LP a RP):         ") (display (parse (ref 'S) '(LP a RP))) (newline)
(display "  parse (LP LP a RP RP):   ") (display (parse (ref 'S) '(LP LP a RP RP))) (newline)
(display "  parse (LP LP a RP):      ") (display (parse (ref 'S) '(LP LP a RP))) (newline)
(newline)

;;; ── Demo 5: Expression evaluator ────────────────────────────────

(display "── Demo 5: Calculator ──") (newline)

;; Right-recursive expression grammar:
;; expr → num + expr | num * expr | num
;; (right-associative, no precedence — but demonstrates recursive parsing)

(define g-num
  (alts (map (lambda (d) (red (lit d) (lambda (x) d)))
             '(0 1 2 3 4 5 6 7 8 9))))

(define-grammar! 'calc
  (alt (red (seq g-num (seq (lit '+) (ref 'calc)))
            (lambda (p) (+ (car p) (cdr (cdr p)))))
       (alt (red (seq g-num (seq (lit '*) (ref 'calc)))
                 (lambda (p) (* (car p) (cdr (cdr p)))))
            g-num)))

(display "  3           = ") (display (parse (ref 'calc) '(3))) (newline)
(display "  3 + 4       = ") (display (parse (ref 'calc) '(3 + 4))) (newline)
(display "  2 * 3 + 4   = ") (display (parse (ref 'calc) '(2 * 3 + 4))) (newline)
(display "  2 + 3 * 4   = ") (display (parse (ref 'calc) '(2 + 3 * 4))) (newline)
(display "  1 + 2 + 3   = ") (display (parse (ref 'calc) '(1 + 2 + 3))) (newline)
(newline)

;;; ── Demo 6: Ambiguous grammar ───────────────────────────────────

(display "── Demo 6: Ambiguous grammar ──") (newline)
(display "  S → A · B,  A → x | x·x,  B → x·x | x") (newline)
(display "  Input 'x x x' should have 2 parses:") (newline)
(display "    (A=x, B=xx)  and  (A=xx, B=x)") (newline)

(define g-A (alt (lit 'x) (lits '(x x))))
(define g-B (alt (lits '(x x)) (lit 'x)))
(define g-amb (seq g-A g-B))

(display "  result: ") (display (parse g-amb '(x x x))) (newline)
(newline)

;;; ── Demo 7: Hash-consing advantage ──────────────────────────────

(display "── Demo 7: Hash-consing sharing ──") (newline)

(define g1 (seq (lit 'a) (lit 'b)))
(define g2 (seq (lit 'a) (lit 'c)))
(define g12 (alt g1 g2))

(define d12 (derive g12 'a))
(display "  d/da(a·b | a·c): ") (write d12) (newline)

;; Both branches yield eps*(a)·(lit b) and eps*(a)·(lit c).
;; The (eps a) node inside is eq?-identical between them.
;; With mk-seq optimization it becomes red(lit b, ...) and red(lit c, ...)
;; but the reduction closures capture the same hash-consed 'a.

;; Derive the same grammar twice → eq?-identical result
(define d12a (derive g12 'a))
(define d12b (derive g12 'a))
(display "  derive(g, a) twice: eq? = ") (display (eq? d12a d12b)) (newline)

;; Show structural sharing in the derived grammar
(display "  Derived grammar nodes: ") (display (heap-size)) (newline)
(gc)
(display "  After GC:              ") (display (heap-size)) (newline)
(newline)

;;; ── Summary ──────────────────────────────────────────────────────

(display "── How hash-consing simplifies derivative parsing ──") (newline)
(display "  1. Grammar nodes are hash-consed: identical derived grammars") (newline)
(display "     are eq?-identical — no explicit memo tables needed.") (newline)
(display "  2. Smart constructors use eq? for free simplification:") (newline)
(display "     alt(g, g) → g is just a pointer comparison.") (newline)
(display "  3. Parse forests share structure automatically.") (newline)
(display "  4. Recursive grammars use named refs with cycle-breaking.") (newline)
