;;; ==========================================================================
;;; kanren-demo.lsp — miniKanren Demonstrations
;;; ==========================================================================

(load "examples/kanren.lsp")

;;; ── Demo 1: Basic Unification ────────────────────────────────────

(display "--- Demo 1: Basic Unification ---") (newline)

(display "  (run 1 (== q 5))           = ")
(display (run 1 (lambda (q) (== q 5))))
(newline)

(display "  (run 1 (== q '(a b c)))    = ")
(display (run 1 (lambda (q) (== q '(a b c)))))
(newline)

(display "  (run 1 (== 3 3))           = ")
(display (run 1 (lambda (q) (conj (== 3 3) (== q 'yes)))))
(newline)

(display "  (run 1 (== 3 4))           = ")
(display (run 1 (lambda (q) (conj (== 3 4) (== q 'yes)))))
(newline)

(newline)

;;; ── Demo 2: Multiple Answers ─────────────────────────────────────

(display "--- Demo 2: Multiple Answers (conde) ---") (newline)

(display "  two answers:   ")
(display (run* (lambda (q)
  (conde2 (== q 'tea) (== q 'coffee)))))
(newline)

(display "  three answers: ")
(display (run* (lambda (q)
  (conde3 (== q 'red) (== q 'green) (== q 'blue)))))
(newline)

(display "  free variable: ")
(display (run* (lambda (q) succeed)))
(newline)

(newline)

;;; ── Demo 3: Fresh Variables and Conjunction ──────────────────────

(display "--- Demo 3: Fresh Variables ---") (newline)

(display "  pair: ")
(display (run* (lambda (q)
  (fresh/2 (lambda (x y)
    (conj* (list (== x 'hello) (== y 'world) (== q (cons x y)))))))))
(newline)

(display "  list: ")
(display (run* (lambda (q)
  (fresh/2 (lambda (x y)
    (conj* (list (== x 1) (== y 2) (== q (list x y)))))))))
(newline)

(newline)

;;; ── Demo 4: appendo — Forward ───────────────────────────────────

(display "--- Demo 4: appendo (Forward) ---") (newline)

(display "  (append '(a b) '(c d)):  ")
(display (run* (lambda (q) (appendo '(a b) '(c d) q))))
(newline)

(display "  (append '() '(x y z)):   ")
(display (run* (lambda (q) (appendo '() '(x y z) q))))
(newline)

(display "  (append '(1) '()):       ")
(display (run* (lambda (q) (appendo '(1) '() q))))
(newline)

(newline)

;;; ── Demo 5: appendo — Backward (the classic!) ──────────────────

(display "--- Demo 5: appendo (Backward) ---") (newline)
(display "  All ways to split (1 2 3):") (newline)

(for-each
  (lambda (answer)
    (display "    ") (display answer) (newline))
  (run* (lambda (q)
    (fresh/2 (lambda (x y)
      (conj* (list (appendo x y '(1 2 3))
                   (== q (list x y)))))))))

(newline)

(display "  What prepended to (3 4) gives (1 2 3 4)?  ")
(display (run* (lambda (q) (appendo q '(3 4) '(1 2 3 4)))))
(newline)

(newline)

;;; ── Demo 6: membero ─────────────────────────────────────────────

(display "--- Demo 6: membero ---") (newline)

(display "  members of (a b c): ")
(display (run* (lambda (q) (membero q '(a b c)))))
(newline)

(display "  is 'b a member?     ")
(display (run 1 (lambda (q) (conj (membero 'b '(a b c)) (== q 'yes)))))
(newline)

(display "  is 'd a member?     ")
(display (run 1 (lambda (q) (conj (membero 'd '(a b c)) (== q 'yes)))))
(newline)

(newline)

;;; ── Demo 7: Infinite Generation with run-n ──────────────────────

(display "--- Demo 7: Infinite Generation ---") (newline)

;; Generate lists of ones: (), (1), (1 1), (1 1 1), ...
(define (oneso q)
  (conde2 (== q '())
          (fresh/1 (lambda (d)
            (conj (== q (cons 1 d)) (oneso d))))))

(display "  first 6 lists of 1s: ")
(display (run 6 (lambda (q) (oneso q))))
(newline)

;; Generate all lists that contain 'x' somewhere
(define (has-x lst)
  (fresh/2 (lambda (h t)
    (conj (== lst (cons h t))
          (conde2 (== h 'x)
                  (has-x t))))))

(display "  first 5 lists with 'x': ")
(display (run 5 (lambda (q) (has-x q))))
(newline)

(newline)

;;; ── Demo 8: Hash-Consing Benefits ──────────────────────────────

(display "--- Demo 8: Hash-Consing Benefits ---") (newline)

;; Run the same query twice — the answer lists are eq?-identical
(define answers1 (run* (lambda (q) (appendo '(a b) '(c d) q))))
(define answers2 (run* (lambda (q) (appendo '(a b) '(c d) q))))

(display "  same query twice eq?:  ")
(display (eq? answers1 answers2))
(newline)

;; Two variables unified to the same structure share the same pointer
(let ((result (run* (lambda (q)
    (fresh/2 (lambda (x y)
      (conj* (list (== x '(a b c d e))
                   (== y '(a b c d e))
                   (== q (list x y))))))))))
  (let ((answer (car result)))
    (display "  unified to same list:   ")
    (display answer)
    (newline)
    (display "  both halves eq?:        ")
    (display (eq? (car answer) (car (cdr answer))))
    (newline)))

(newline)
(display "All demos passed.") (newline)
