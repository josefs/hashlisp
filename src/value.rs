///! Value representation for Hashlisp.
///!
///! We use NaN-boxing: a 64-bit value that is either a plain f64 or encodes
///! other types in the unused NaN payload bits.
///!
///! IEEE 754 double layout:
///!   [S][EEEEEEEEEEE][MMMM...52 bits...MMMM]
///!
///! A quiet NaN has exponent = all 1s and mantissa bit 51 = 1.
///! We set bit 50 as well ("our" NaN vs hardware NaN) leaving 50 payload bits.
///!
///! Payload layout (50 bits):
///!   [TTTT][VVVVVVVV...46 bits...VVVVVVVV]
///!
///! Tag (4 bits, values 0..15):
///!   0  = integer (i46, sign-extended from 46 bits)
///!   1  = boolean (0 or 1)
///!   2  = nil
///!   3  = char (unicode scalar, 21 bits used)
///!   4  = symbol id (index into symbol table)
///!   5  = heap hash reference (46-bit hash, indexes into hash-consed heap)
///!   6  = builtin function id
///!   7  = void (unspecified return)
///!
///! Primitives (int, bool, nil, char, symbol, builtin, void) are *immediate*
///! — they live entirely inside the 64-bit word with no heap allocation.
///! Only tag 5 (heap ref) points into the hash-consed heap.

use std::fmt;

// Quiet NaN with bit 50 set = our "signal" prefix.
// Bits 62..52 = exponent (all 1s), bit 51 = quiet NaN, bit 50 = ours.
const NANISH: u64 = 0x7FFC_0000_0000_0000;

// Tag lives in bits 49..46 (4 bits).
const TAG_SHIFT: u64 = 46;
const TAG_MASK: u64 = 0xF;
const PAYLOAD_MASK: u64 = (1u64 << 46) - 1; // 46 bits

// Tags
pub const TAG_INT: u64 = 0;
pub const TAG_BOOL: u64 = 1;
pub const TAG_NIL: u64 = 2;
pub const TAG_CHAR: u64 = 3;
pub const TAG_SYMBOL: u64 = 4;
pub const TAG_HEAP: u64 = 5;
pub const TAG_BUILTIN: u64 = 6;
pub const TAG_VOID: u64 = 7;

/// A Hashlisp value — 64 bits, NaN-boxed.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Val(pub u64);

impl Val {
    // ── Constructors ──

    #[inline]
    pub fn float(f: f64) -> Val {
        let bits = f.to_bits();
        // Make sure a hardware NaN doesn't collide with our tagging.
        // All real NaNs have exponent all-1s; we require bit 50 clear for "real" NaN.
        debug_assert!(
            !f.is_nan() || (bits & NANISH) != NANISH,
            "hardware NaN collides with NaN-boxing"
        );
        Val(bits)
    }

    #[inline]
    fn tagged(tag: u64, payload: u64) -> Val {
        Val(NANISH | (tag << TAG_SHIFT) | (payload & PAYLOAD_MASK))
    }

    #[inline]
    pub fn int(i: i64) -> Val {
        // Truncate to 46 bits (sign-extended on extraction)
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
    pub fn is_nanboxed(self) -> bool {
        (self.0 & NANISH) == NANISH
    }

    #[inline]
    pub fn tag(self) -> Option<u64> {
        if self.is_nanboxed() {
            Some((self.0 >> TAG_SHIFT) & TAG_MASK)
        } else {
            None // it's a float
        }
    }

    #[inline]
    pub fn payload(self) -> u64 {
        self.0 & PAYLOAD_MASK
    }

    // ── Extractors ──

    #[inline]
    pub fn as_float(self) -> Option<f64> {
        if !self.is_nanboxed() {
            Some(f64::from_bits(self.0))
        } else {
            None
        }
    }

    #[inline]
    pub fn as_int(self) -> Option<i64> {
        if self.tag() == Some(TAG_INT) {
            let raw = self.payload();
            // Sign-extend from 46 bits
            let shift = 64 - 46;
            Some(((raw << shift) as i64) >> shift)
        } else {
            None
        }
    }

    #[inline]
    pub fn as_bool(self) -> Option<bool> {
        if self.tag() == Some(TAG_BOOL) {
            Some(self.payload() != 0)
        } else {
            None
        }
    }

    #[inline]
    pub fn is_nil(self) -> bool {
        self.tag() == Some(TAG_NIL)
    }

    #[inline]
    pub fn as_char(self) -> Option<char> {
        if self.tag() == Some(TAG_CHAR) {
            char::from_u32(self.payload() as u32)
        } else {
            None
        }
    }

    #[inline]
    pub fn as_symbol(self) -> Option<u32> {
        if self.tag() == Some(TAG_SYMBOL) {
            Some(self.payload() as u32)
        } else {
            None
        }
    }

    #[inline]
    pub fn as_heap_ref(self) -> Option<u64> {
        if self.tag() == Some(TAG_HEAP) {
            Some(self.payload())
        } else {
            None
        }
    }

    #[inline]
    pub fn as_builtin(self) -> Option<u32> {
        if self.tag() == Some(TAG_BUILTIN) {
            Some(self.payload() as u32)
        } else {
            None
        }
    }

    #[inline]
    pub fn is_void(self) -> bool {
        self.tag() == Some(TAG_VOID)
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
        self.tag() == Some(TAG_HEAP)
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(fl) = self.as_float() {
            write!(f, "Float({fl})")
        } else {
            match self.tag() {
                Some(TAG_INT) => write!(f, "Int({})", self.as_int().unwrap()),
                Some(TAG_BOOL) => write!(f, "Bool({})", self.as_bool().unwrap()),
                Some(TAG_NIL) => write!(f, "Nil"),
                Some(TAG_CHAR) => write!(f, "Char({:?})", self.as_char().unwrap()),
                Some(TAG_SYMBOL) => write!(f, "Sym({})", self.payload()),
                Some(TAG_HEAP) => write!(f, "Heap(0x{:012x})", self.payload()),
                Some(TAG_BUILTIN) => write!(f, "Builtin({})", self.payload()),
                Some(TAG_VOID) => write!(f, "Void"),
                _ => write!(f, "Unknown(0x{:016x})", self.0),
            }
        }
    }
}
