# Value Representation

Hashlisp packs every value into a single 64-bit word using a tagged
representation.

## Layout

```
 63      61  60                                            0
┌─────────┬────────────────────────────────────────────────┐
│  TAG(3) │              PAYLOAD (61 bits)                 │
└─────────┴────────────────────────────────────────────────┘
```

| Tag | Type | Payload |
|-----|------|---------|
| 0 | Integer | i61, sign-extended |
| 1 | Boolean | 0 or 1 |
| 2 | Nil | unused (0) |
| 3 | Char | Unicode scalar (21 bits used) |
| 4 | Symbol | Index into the symbol table |
| 5 | Heap ref | 61-bit hash into the hash-consed heap |
| 6 | Builtin | Builtin function id |
| 7 | Void | unused (0) |

All types except heap refs are **immediate** — they live entirely inside
the 64-bit word with no heap allocation.

## Why no floats?

An earlier version used NaN-boxing: real IEEE 754 doubles were stored
directly, and other types were encoded in the unused NaN payload bits.
This left only a 4-bit tag and **46-bit payload** for non-float values.

Removing float support freed up all 64 bits for a simple tagged scheme:
3 bits for the tag, **61 bits for the payload**. Since the payload
directly determines hash-collision resistance, this matters a lot for
a hash-consing language.

## Birthday-paradox analysis

Hash collisions become likely (≈50% probability) when the number of
distinct objects reaches approximately √(2^n), where n is the hash width.

| | 46-bit (old) | 61-bit (current) |
|---|---|---|
| Hash space | 2^46 ≈ 70 trillion | 2^61 ≈ 2.3 quintillion |
| 50% collision threshold | 2^23 ≈ **8.4 million** | 2^30.5 ≈ **1.5 billion** |
| Improvement | | **~181×** more objects before collision risk |

With the old 46-bit hashes, a program with tens of millions of
hash-consed nodes had a real chance of a spurious collision. With 61 bits,
you need over a billion distinct heap objects to reach the same risk level.

## Integer range

The wider payload also increases the integer range from i46
(±35 trillion) to i61 (±1.15 quintillion), which is close to i64
and sufficient for virtually all practical computations.
