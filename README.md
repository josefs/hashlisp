# Hashlisp

A Scheme-like language where the heap is a hash table.

Every cons cell, string, and vector is **hash-consed**: if two structures
have identical contents they share the same heap slot and are `eq?` in O(1).
This single design choice turns out to be surprisingly powerful — BDD unique
tables, memoization keys, set equality, and structural deduplication all
come for free.

## Build & Run

```
cargo build
cargo run                          # REPL
cargo run -- examples/bdd-demo.lsp # run a file
```

## Why is hash-consing cool?

Traditional Lisps allocate a fresh cell for every `cons`. Hashlisp doesn't:

```scheme
hashlisp> (eq? (cons 1 2) (cons 1 2))
#t
```

Two calls to `cons` with the same arguments return the *same pointer*.
This means:

- **Structural equality is pointer equality.** No need to walk trees.
- **Identical subtrees share memory.** A BDD with 10^6 nodes but only
  10^4 unique structures uses 10^4 cells.
- **`define-memo`** This form defines functions that are memoized. Since identical  arguments hash-cons to the same key, memoization is particularly fast and useful.

Some cool libraries that are implemented in hashlisp:
- The sets library uses a form of hash tries which always have the same structure for the same elements. This means that set equality can be done very efficiently by comparing the hash of the two sets. 
- The BDD library can be implemented as if we just operate on trees. Common subtrees will be autmatically shared. By also memoizing functions, we get a fully working BDD implementation with very little effort. 
- E-graphs make crucial use of sharing through hash-consing. Hashlisp includes an equality saturation library with an implementation of e-graphs.
- When writing DSLs, a common problem is that terms are duplicated and the resulting code explode in size. Terms are never duplicated in hashlisp and the `let-insertion` library shows how to insert let-expressions for shared terms, which is useful for e.g. deserializing terms.
- The hashlife library implements Gosper's algorithm to speed up the Game of Life which uses quadtrees and sharing. The sharing is automatic in hashlisp. 


## Language

Hashlisp is a dynamically typed Scheme dialect. Values are tagged
into 64-bit words — integers, booleans, characters, symbols,
and heap references all fit in a single machine word. A 3-bit tag
leaves 61 bits of payload for hashes.

### Special Forms

`define`, `lambda`, `if`, `cond`, `let`, `let*`, `letrec`, `begin`,
`set!`, `and`, `or`, `when`, `unless`, `quote`, `quasiquote`,
`define-macro`, `define-memo`, `apply`

### Builtins (83)

| Category | Functions |
|---|---|
| Arithmetic | `+` `-` `*` `/` `=` `<` `>` `<=` `>=` `modulo` `remainder` `abs` `min` `max` `expt` `sqrt` `floor` `ceiling` `truncate` `round` |
| Predicates | `null?` `pair?` `list?` `number?` `symbol?` `string?` `boolean?` `char?` `procedure?` `vector?` `void?` `zero?` `positive?` `negative?` `even?` `odd?` |
| Pairs & Lists | `cons` `car` `cdr` `cadr` `caddr` `cddr` `caar` `cdar` `list` `length` `append` `reverse` |
| Higher-Order | `map` `filter` `fold` `for-each` `apply` |
| Strings | `string-length` `string-ref` `string-append` `substring` `string->list` `list->string` `number->string` `string->number` `display-string` |
| Characters | `char->integer` `integer->char` |
| Vectors | `vector` `vector-ref` `vector-length` `vector->list` `list->vector` |
| I/O | `display` `write` `newline` `load` `error` |
| Equality | `eq?` `equal?` |
| Logic | `not` |
| Hash-consing | `hash-of` `heap-size` `gc` `gensym` |
| Macros | `macroexpand` |
| Control | `void` |

### Macros

- Hashlisp as a simple macro system with `define-macro` and `gensym`.

### Extensions Beyond Scheme

- **`define-memo`** — like `define` but the function's results are cached.
  The cache key is the argument tuple, which works because hash-consing
  makes structurally identical arguments pointer-equal.
- **`hash-of`** — expose the internal hash of any value.
- **`gensym`** — generate a unique symbol (for hygienic macros, sentinel tags).
- **`heap-size` / `gc`** — introspect and control the garbage collector.

## Examples

The `examples/` directory contains libraries and demos that showcase what
hash-consing enables:

| File | What it does |
|---|---|
| [hash-consing-demo.lsp](examples/hash-consing-demo.lsp) | Core hash-consing properties and `eq?` identity |
| [bdd.lsp](examples/bdd.lsp) | BDD library — sharing is free due to hash-consing |
| [ctl.lsp](examples/ctl.lsp) | Symbolic CTL model checker built on BDDs |
| [eqsat.lsp](examples/eqsat.lsp) | Equality saturation with e-graphs |
| [hashlife.lsp](examples/hashlife.lsp) | Gosper's HashLife — quadtree hashing and sharing is free |
| [sets.lsp](examples/sets.lsp) | Functional sets & maps via hash tries with canonical `eq?` |
| [kanren.lsp](examples/kanren.lsp) | miniKanren logic programming |
| [pwz.lsp](examples/pwz.lsp) | Parsing with Zippers (Brzozowski derivatives) |
| [let-insertion.lsp](examples/let-insertion.lsp) | DAG→let* serialization + symbolic differentiation |
| [metacircular.lsp](examples/metacircular.lsp) | Metacircular evaluator |
| [church.lsp](examples/church.lsp) | Church numerals in pure lambda calculus |
| [streams.lsp](examples/streams.lsp) | Lazy streams |
| [mergesort.lsp](examples/mergesort.lsp) | Merge sort |
| [quine.lsp](examples/quine.lsp) | A quine |

Each file is relatively well documented.

## Implementation

~2500 lines of Rust, no dependencies beyond `rustyline` for the REPL.

| Module | Role |
|---|---|
| `value.rs` | Tagged 64-bit value representation (3-bit tag, 61-bit payload) |
| `heap.rs` | Hash-consed heap (FNV-1a, per-type salts) + mark-and-sweep GC |
| `symbol.rs` | Interned symbol table |
| `parser.rs` | S-expression lexer and parser |
| `eval.rs` | Tree-walking evaluator with tail-call optimization |
| `printer.rs` | Value pretty-printer |
| `env.rs` | Environment frames with parent chain |

### Garbage Collection

Mark-and-sweep with roots from:
- The environment chain
- The shadow stack (protects intermediate values during builtin calls)
- Extra roots (parsed AST nodes)
- Macro definitions
- `define-memo` cache tables

### Hash Function

FNV-1a with per-type salts (`SALT_CONS`, `SALT_STRING`, `SALT_VECTOR`,
`SALT_CLOSURE`) folded into a 61-bit payload that fits the tagged word.
Closures get monotonic IDs (they capture mutable environments, so they
can't be shared by structure).

## History

The idea of using Hash consing in Lisp is an old one. It dates back at
least to Eliichi Goto's [^1] HLISP, outlined in [Monocopy and Associative Algorithms in an Extended Lisp](https://www.cs.utexas.edu/~hunt/research/hash-cons/hash-cons-papers/monocopy-goto.pdf)


[^1]: A computer scientist whose name is Goto is just *chef's kiss*.