# Tail Calls, Memoization, and Interpreters

When building a metacircular interpreter in Hashlisp, tail calls
and memoization interact in subtle ways.  This
document explains why the base interpreter needs no trampoline while
the memoized version does, and how memoization still works despite the
trampoline's indirection.

## Background: how Hashlisp handles tail calls

Hashlisp's Rust evaluator uses an explicit trampoline.  The inner
evaluator returns either `Done(value)` or `TailCall { expr, env }`,
and a loop in `eval_in` keeps iterating until it gets a `Done`.  This
means any Hashlisp function that makes a recursive call in tail
position — the last thing the function does — runs in constant stack
space.

```scheme
;; Proper tail calls makes this O(1) stack:
(define (sum-to n acc)
  (if (= n 0) acc
      (sum-to (- n 1) (+ n acc))))
```

## A trampoline-free metacircular interpreter

Because the host provides proper tail calls, a metacircular interpreter written in
Hashlisp can be direct-recursive.  Consider the core of the evaluator:

```scheme
(define (my-eval expr env)
  (cond
    ...
    ;; if: evaluate test, then tail-call my-eval on the branch
    ((eq? head 'if)
     (if (my-eval (cadr expr) env)
         (my-eval (caddr expr) env)       ;; tail position ✓
         (my-eval (cadddr expr) env)))     ;; tail position ✓
    ...))
```

When `my-eval` calls itself in tail position, the host evaluator will not
use any stack space.  The chain `my-eval → apply-fn → my-eval`
is entirely in tail position, so the host's trampoline handles it —
no explicit trampoline needed at the meta-level.

A meta-program running sum-to-1000 inside this interpreter works fine:
```scheme
(my-eval '(let loop ((i 1000) (acc 0))
            (if (= i 0) acc
                (loop (- i 1) (+ acc i))))
         init-env)
;; => 500500, with O(1) host stack usage
```

## `define-memo` breaks tail calls

Hashlisp's `define-memo` creates a wrapper that checks a cache before
calling the function body and stores the result afterward:

```
memo-wrapper(args):
  key = hash(args)
  if key in cache: return cache[key]
  result = real-function(args)     ← NOT a tail call
  cache[key] = result
  return result
```

The call to `real-function` is not in tail position because the
wrapper must do work (store in cache) after the function returns.
This means every recursive call through a `define-memo` function
consumes a Rust stack frame.  For deep recursion like evaluating a
1000-iteration loop, this would overflow the stack.

## The memoized interpreter needs its own trampoline

To regain proper tail calls, the memoized version of the interpreter
introduces an explicit trampoline at the meta-level:

```scheme
;; Trampoline tags
(define (make-done val)      (cons 'done val))
(define (make-tail expr env) (cons 'tail-call (cons expr env)))

;; The memoized core returns tags, not values
(define-memo (eval-inner expr env)
  (cond
    ...
    ((eq? head 'if)
     (if (my-eval (cadr expr) env)
         (make-tail (caddr expr) env)       ;; return a tag
         (make-tail (cadddr expr) env)))     ;; return a tag
    ...))

;; The trampoline loop — host tail calls keeps this O(1) stack
(define (trampoline r)
  (if (done? r) (done-val r)
      (trampoline (eval-inner (tc-expr r) (tc-env r)))))

(define (my-eval expr env)
  (trampoline (eval-inner expr env)))
```

The key insight is that `trampoline` itself is a regular (non-memo)
function, so the host's TCO applies to its self-calls.  The memoized
`eval-inner` never recurses directly — it returns a tag, and the
non-memoized `trampoline` iterates.  This keeps the stack bounded.

## How memoization still works with a trampoline

At first glance, the trampoline seems to weaken memoization. Without
it, a cache hit for `(expr, env)` returns the final value directly.
With it, a cache hit might return a `tail-call` tag that the
trampoline must still follow.

But the memoization benefit is preserved.  Consider Fibonacci:

```scheme
(letrec ((fib (lambda (n)
                (if (< n 2) n
                    (+ (fib (- n 1)) (fib (- n 2)))))))
  (fib 30))
```

Without memoization, `fib(30)` makes ~2³⁰ calls.  With `eval-inner`
memoized, each unique `(expr, env)` pair is computed at most once.
On a cache hit, the trampoline may need to follow one or two tags, but
this is O(1) work per hit — the exponential→linear transformation is
fully preserved.

Concretely, when `fib(8)` is needed a second time:

1. `eval-inner(fib-body, {n→8})` — cache hit → `(tail-call plus-expr, {n→8})`
2. `eval-inner(plus-expr, {n→8})` — cache hit → `(done . 21)`

Two constant-time lookups instead of one.  The total number of distinct
`(expr, env)` pairs for `fib(n)` is still O(n), each visited once.

## Hash-consing makes memo keys cheap

The reason this works so well in Hashlisp is that environments are
hash-consed.  When `fib(8)` is called from two different sites — once
as `fib(9-1)` and once as `fib(10-2)` — the number 8 is the same
immediate value, and the resulting environment `{n→8, fib→closure}`
is the same hash-consed cons chain.  The memo key matches by pointer
equality (`eq?`), making lookup O(1).

In a language without hash-consing, memo keys would require structural
comparison, making the overhead much higher.  Hash-consing turns the
metacircular interpreter's memo cache into something more like a
hardware cache: cheap to probe, with high hit rates for recursive
programs.

## Summary

| Interpreter     | Trampoline | Tail call mechanism              | Memoizable |
|-----------------|------------|----------------------------------|------------|
| Base            | No         | Host tail calls (free)           | N/A        |
| Memoized        | Yes        | Explicit tags + host tail calls  | ✓          |

The base interpreter relies entirely on the host's tail-call
implementation.  The memoized interpreter cannot do so (because
`define-memo` wrappers break tail position) and must introduce its own
trampoline.  The trampoline's iteration loop is itself tail-recursive
in the host, keeping the overall stack usage bounded.  Memoization
remains effective because each `(expr, env)` pair is still computed
only once — cache hits may return tags rather than final values, but
resolving a tag is constant-time work.
