///! Value representation for Hashlisp.
///!
///! Tagged 64-bit representation (no floating point):
///!
///!   [TTT][VVVVVVVV...61 bits...VVVVVVVV]
///!
///! Tag (3 bits, bits 63..61):
///!   0  = integer (i61, sign-extended from 61 bits)
///!   1  = boolean (0 or 1)
///!   2  = nil
///!   3  = char (unicode scalar, 21 bits used)
///!   4  = symbol id (index into symbol table)
///!   5  = heap hash reference (61-bit hash, indexes into hash-consed heap)
///!   6  = builtin function id
///!   7  = void (unspecified return)
///!
///! Primitives (int, bool, nil, char, symbol, builtin, void) are *immediate*
///! — they live entirely inside the 64-bit word with no heap allocation.
///! Only tag 5 (heap ref) points into the hash-consed heap.

use std::fmt;

// Tag lives in bits 63..61 (3 bits).
const TAG_SHIFT: u64 = 61;
const TAG_MASK: u64 = 0x7;
pub const PAYLOAD_MASK: u64 = (1u64 << 61) - 1; // 61 bits

// Tags
pub const TAG_INT: u64 = 0;
pub const TAG_BOOL: u64 = 1;
pub const TAG_NIL: u64 = 2;
pub const TAG_CHAR: u64 = 3;
pub const TAG_SYMBOL: u64 = 4;
pub const TAG_HEAP: u64 = 5;
pub const TAG_BUILTIN: u64 = 6;
pub const TAG_VOID: u64 = 7;

/// A Hashlisp value — 64 bits, tagged.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Val(pub u64);

impl Val {
    // ── Constructors ──

    #[inline]
    fn tagged(tag: u64, payload: u64) -> Val {
        Val((tag << TAG_SHIFT) | (payload & PAYLOAD_MASK))
    }

    #[inline]
    pub fn int(i: i64) -> Val {
        // Truncate to 61 bits (sign-extended on extraction)
        let payload = (i as u64) & PAYLOAD_MASK;
        Self::tagged(TAG_INT, payload)
    }

    #[inline]
    pub fn boolean(b: bool) -> Val {
        Self::tagged(TAG_BOOL, b as u64)
    }

    #[inline]
    pub fn nil() -> Val {
        Self::tagged(TAG_NIL, 0)
    }

    #[inline]
    pub fn char_(c: char) -> Val {
        Self::tagged(TAG_CHAR, c as u64)
    }

    #[inline]
    pub fn symbol(id: u32) -> Val {
        Self::tagged(TAG_SYMBOL, id as u64)
    }

    #[inline]
    pub fn heap_ref(hash: u64) -> Val {
        Self::tagged(TAG_HEAP, hash & PAYLOAD_MASK)
    }

    #[inline]
    pub fn builtin(id: u32) -> Val {
        Self::tagged(TAG_BUILTIN, id as u64)
    }

    #[inline]
    pub fn void() -> Val {
        Self::tagged(TAG_VOID, 0)
    }

    // ── Queries ──

    #[inline]
    pub fn tag(self) -> u64 {
        (self.0 >> TAG_SHIFT) & TAG_MASK
    }

    #[inline]
    pub fn payload(self) -> u64 {
        self.0 & PAYLOAD_MASK
    }

    // ── Extractors ──

    #[inline]
    pub fn as_int(self) -> Option<i64> {
        if self.tag() == TAG_INT {
            let raw = self.payload();
            // Sign-extend from 61 bits
            let shift = 64 - 61;
            Some(((raw << shift) as i64) >> shift)
        } else {
            None
        }
    }

    #[inline]
    pub fn as_bool(self) -> Option<bool> {
        if self.tag() == TAG_BOOL {
            Some(self.payload() != 0)
        } else {
            None
        }
    }

    #[inline]
    pub fn is_nil(self) -> bool {
        self.tag() == TAG_NIL
    }

    #[inline]
    pub fn as_char(self) -> Option<char> {
        if self.tag() == TAG_CHAR {
            char::from_u32(self.payload() as u32)
        } else {
            None
        }
    }

    #[inline]
    pub fn as_symbol(self) -> Option<u32> {
        if self.tag() == TAG_SYMBOL {
            Some(self.payload() as u32)
        } else {
            None
        }
    }

    #[inline]
    pub fn as_heap_ref(self) -> Option<u64> {
        if self.tag() == TAG_HEAP {
            Some(self.payload())
        } else {
            None
        }
    }

    #[inline]
    pub fn as_builtin(self) -> Option<u32> {
        if self.tag() == TAG_BUILTIN {
            Some(self.payload() as u32)
        } else {
            None
        }
    }

    #[inline]
    pub fn is_void(self) -> bool {
        self.tag() == TAG_VOID
    }

    /// Returns true if this value is "truthy" in Scheme sense.
    /// Only #f is falsy; everything else (including 0, nil) is truthy.
    #[inline]
    pub fn is_truthy(self) -> bool {
        self.as_bool() != Some(false)
    }

    /// Is this a heap reference?
    #[inline]
    pub fn is_heap(self) -> bool {
        self.tag() == TAG_HEAP
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.tag() {
            TAG_INT => write!(f, "Int({})", self.as_int().unwrap()),
            TAG_BOOL => write!(f, "Bool({})", self.as_bool().unwrap()),
            TAG_NIL => write!(f, "Nil"),
            TAG_CHAR => write!(f, "Char({:?})", self.as_char().unwrap()),
            TAG_SYMBOL => write!(f, "Sym({})", self.payload()),
            TAG_HEAP => write!(f, "Heap(0x{:016x})", self.payload()),
            TAG_BUILTIN => write!(f, "Builtin({})", self.payload()),
            TAG_VOID => write!(f, "Void"),
            _ => write!(f, "Unknown(0x{:016x})", self.0),
        }
    }
}
