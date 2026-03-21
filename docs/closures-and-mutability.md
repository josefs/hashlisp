# Closures, Mutability, and Sharing

Hashlisp does not provide `set!`.  This is a deliberate design choice
that enables closures to be hash-consed — structurally identical
closures are shared automatically.

## How closure sharing works

A closure is a triple *(params, body, env)*.  In traditional Scheme,
the environment is a mutable frame, so two closures that happen to
close over the same variables can never be pointer-equal: each
allocation produces a fresh frame.

In Hashlisp, environments are immutable association lists built from
hash-consed cons cells.  Two closures with the same parameters, body,
and captured environment will hash to the same heap slot and be `eq?`.
The runtime does not need to traverse the closure body to determine
which variables are free — it captures the entire environment chain,
and sharing comes for free from the hash-consed representation.

## What `set!` would break

If a closure could mutate a variable in its captured environment,
then structurally identical closures would behave differently depending
on mutation history.  Identity (`eq?`) could no longer be based on
structure alone.

Concretely, sharing would fail whenever a closure captures a mutable
binding:

```scheme
;; With set!, these two closures could NOT be shared:
(define (make-counter)
  (let ((n 0))
    (lambda () (set! n (+ n 1)) n)))

(define c1 (make-counter))
(define c2 (make-counter))
;; c1 and c2 would need independent copies of n
```

Without `set!`, `c1` and `c2` in a similar program would be
structurally identical and automatically shared.

## Two-layer environment design

Hashlisp uses a two-layer approach to variable lookup:

1. **Local environment** — an immutable, hash-consed alist captured by
   closures.  Bindings are prepended via cons; lookup walks the chain.
   Because the alist is built from hash-consed cells, two closures that
   capture identical bindings share the same environment.

2. **Global table** — a mutable `HashMap` for top-level `define`
   bindings.  Globals are *not* captured by closures; they are looked
   up as a fallback when a symbol is not found in the local alist.

This split lets `define` support recursive functions (the closure
can refer to its own name via the global table, even though the
local alist was created before the define) while keeping the
closure's captured environment immutable and shareable.

## Recursive local closures

Named `let` creates a local recursive closure.  Since the closure
needs to refer to itself, Hashlisp stores a `self_name` field in the
closure object.  When the closure is applied, the runtime
automatically binds `self_name` to the closure in the call
environment, enabling recursion without mutating the captured env.

```scheme
(let loop ((i 0))
  (if (< i 10)
      (loop (+ i 1))    ;; loop is bound by the self_name mechanism
      i))
```

## Global state without `set!`

Programs that need mutable global state can use `define` to redefine
a top-level binding.  This does not affect closure sharing: closures
never capture the global table, so redefining a global does not
invalidate any shared closure.

```scheme
(define *counter* 0)
(define (bump!) (define *counter* (+ *counter* 1)))
(bump!)  ;; *counter* is now 1
```

This is idiomatic in Hashlisp for global accumulators, caches, and
similar patterns.

## Tradeoffs

| | With `set!` | Without `set!` (Hashlisp) |
|---|---|---|
| **Closure identity** | Each closure allocation is unique | Structurally identical closures are `eq?` |
| **Memoization** | Closures cannot be memo keys | Closures can be memo keys (deterministic hash) |
| **Environment cost** | O(1) mutation via frames | O(n) shadowing via alist prepend |
| **Variable lookup** | O(depth) frame walk | O(bindings) alist walk + global fallback |
| **Local mutation** | Direct `set!` | Functional accumulator / named `let` |
| **Global mutation** | `set!` | `define` (redefines global binding) |
| **GC pressure** | Frames are mutable, not shared | Env alists are shared, but more cons cells |

The environment representation trades mutation speed for sharing.
In programs that build many similar closures (e.g. lazy streams,
continuation-based search, BDDs), the sharing benefit outweighs the
overhead of alist construction.

## Flat closures: an alternative not taken

An alternative approach would be *closure conversion*: analyze each
lambda to find its free variables and capture only those.  This
produces smaller environments and enables sharing when closures use
the same subset of variables.

Hashlisp does not do this because:

- It requires traversing the entire lambda body at closure-creation
  time, adding overhead proportional to the body size.
- The hash-consed whole-environment approach already provides sharing
  without program analysis — identical environments are automatically
  deduplicated.
- The simplicity of capturing the full alist keeps the implementation
  small (~30 lines for the environment module).
