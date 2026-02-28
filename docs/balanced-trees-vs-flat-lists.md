# Balanced Trees vs Flat Lists on a Hash-Consed Heap

## Background

On a hash-consed heap, the choice of how to group fields into cons cells affects how much sub-structural sharing the heap can exploit. When storing a fixed-size record as a list, the intermediate tails are often meaningless — but when stored as a balanced tree of cons cells, the sub-pairs can correspond to semantically meaningful groupings that recur across different records.

This was observed in HashLife and then applied to BDDs.

## Two representations

### Flat list: `(nw ne sw se)`

```
(nw . (ne . (sw . (se . nil))))
```

- **4 cons cells** per node (plus nil)
- Intermediate tails `(ne sw se)` and `(sw se)` group spatially unrelated quadrants
- These tails are unlikely to recur across different nodes → little sub-structural sharing

### Balanced tree: `((nw . ne) . (sw . se))`

```
((nw . ne) . (sw . se))
```

- **3 cons cells** per node (no nil terminator)
- Sub-pair `(nw . ne)` = north half of the grid
- Sub-pair `(sw . se)` = south half of the grid
- These halves are spatially meaningful and frequently recur (e.g. in sparse patterns with empty borders)

## Empirical results

Running the full HashLife demo (blinker, glider, R-pentomino, acorn, hash-consing tests):

| Metric | Flat list | Balanced tree | Change |
|---|---|---|---|
| Heap size (before GC) | 46,360 | 37,471 | **−19%** |
| Heap size (after GC) | 34,014 | 22,402 | **−34%** |

## Why the balanced tree wins

1. **25% fewer cons cells per node** — 3 instead of 4, since there's no nil terminator.

2. **Sub-structural sharing of spatial halves** — Two different quadtree nodes that share the same north half (same NW and NE children) automatically share the same hash-consed `(nw . ne)` pair. Similarly for south halves. This is common in Game of Life patterns where large regions are empty: many nodes have `(empty . empty)` as their north or south half.

3. **Cascading benefit** — The shared sub-pairs mean that the hash-consed unique table has fewer distinct entries, which reduces memory pressure and GC work.

The flat list's intermediate tails (`(ne sw se)`, `(sw se)`) mix children from different spatial regions. There's no reason for two unrelated nodes to share these tails, so hash-consing provides no sub-structural benefit.

## Accessor performance

Both representations provide O(1) access to all four children:

| Quadrant | Flat list | Balanced tree |
|---|---|---|
| NW | `(car n)` | `(caar n)` |
| NE | `(cadr n)` | `(cdar n)` |
| SW | `(caddr n)` | `(cadr n)` |
| SE | `(cadddr n)` | `(cddr n)` |

The balanced tree accessors are actually slightly cheaper since the deepest path is 2 pointer dereferences (vs 4 for SE in the flat list).

## Takeaway

When designing data structures on a hash-consed heap, prefer balanced binary groupings over flat lists. Arrange sub-structures so that frequently-shared portions are grouped into the same cons cell — this lets hash-consing provide sub-structural sharing automatically.

---

## Case Study 2: BDD Nodes

### The data structure

A BDD node stores three fields: `(var low high)` where `var` is a variable index, `low` is the subtree when the variable is false, and `high` is the subtree when the variable is true.

### Flat list: `(var low high)`

```
(var . (low . (high . nil)))
```

- **3 cons cells** per node (plus nil)
- The tail `(low . (high . nil))` is a meaningless grouping — it does not correspond to any semantic concept in BDD theory

### Tree: `(var . (low . high))`

```
(var . (low . high))
```

- **2 cons cells** per node (33% fewer)
- The sub-pair `(low . high)` is the **decision pair** — the two branches taken when the variable is true vs false
- BDD nodes for different variables that happen to make the same branching decision share this sub-pair automatically

### Empirical results

Running the full BDD demo (XOR chains up to n=14, De Morgan, distributivity, sat counting):

| Metric | Flat list | Balanced tree | Change |
|---|---|---|---|
| Heap size (before GC) | 1,617 | 1,489 | **−8%** |
| Heap size (after GC) | 1,143 | 977 | **−15%** |

The improvement is smaller than HashLife because BDD nodes only have 3 fields (saving 1 cons cell) vs HashLife's 4 fields (saving 1 cons cell + nil). But the 15% GC reduction still shows meaningful sub-structural sharing of decision pairs.

### Accessor mapping

| Field | Flat list | Balanced tree |
|---|---|---|
| var | `(car b)` | `(car b)` |
| low | `(cadr b)` | `(cadr b)` |
| high | `(caddr b)` | `(cddr b)` |

The `high` accessor drops from 3 pointer dereferences to 2.

---

## Case Study 3: E-graph (Equality Saturation)

### Analysis

The equality saturation library was analyzed but the tree trick does **not** apply, for several reasons:

1. **E-nodes are variable-length** — `(op child-id ...)` has 1+ children depending on the operator's arity. There is no fixed-size tuple to restructure.

2. **Alist entries are already pairs** — `(key . value)` is a single cons cell, already optimal.

3. **Rules are already pairs** — `(lhs . rhs)` is a single cons cell.

4. **Substitutions are alists** — variable-length lists of pairs; no fixed-size structure to reshape.

The trick specifically helps when you have **fixed-arity records stored as lists**, where eliminating the nil terminator and creating meaningful sub-groupings can improve sharing. Variable-length structures must remain as lists.

---

## General Principle

The optimization applies when:

- You have a **fixed-size record** of N fields stored on a hash-consed heap
- You can identify **meaningful sub-groupings** of fields that are likely to recur across different instances
- The record is **frequently allocated** (so the per-node savings add up)

It does **not** apply when:

- The structure is **variable-length** (must remain a list)
- The structure is **already a pair** (already optimal)
- Sub-groupings have **no semantic meaning** (sharing is unlikely regardless)
