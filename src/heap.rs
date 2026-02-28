///! Hash-consed heap.
///!
///! Every compound value (cons, string, closure, vector) lives on the heap,
///! keyed by a 46-bit hash derived from its contents.  Before allocating, we
///! compute the hash and check if an identical value already exists.  If so we
///! return the existing hash — this is "hash-consing".
///!
///! Hash computation:
///!   cons(a, b)  → mix(a.0, b.0, SALT_CONS)
///!   string(s)   → fnv1a(bytes(s)) ⊕ SALT_STRING
///!   closure     → monotonic id (closures are never shared)
///!   vector      → mix of element hashes ⊕ SALT_VECTOR
///!
///! Garbage collection: mark-and-sweep from a set of root Val's.

use std::collections::HashMap;

use crate::value::Val;

// ── Salts for different heap object kinds ──
const SALT_CONS: u64 = 0xBEEF_CAFE_1234;
const SALT_STRING: u64 = 0xDEAD_F00D_5678;
const SALT_CLOSURE: u64 = 0xC105_ED00_9ABC;
const SALT_VECTOR: u64 = 0xBADC_0FFE_E135;

const PAYLOAD_MASK: u64 = (1u64 << 46) - 1;

/// The kinds of objects stored on the hash-consed heap.
#[derive(Debug, Clone)]
pub enum HeapObject {
    /// (car, cdr)
    Cons(Val, Val),
    /// Immutable string
    Str(String),
    /// Closure: (param_names as symbol ids, body as Val (list), captured env id)
    Closure {
        params: Vec<u32>,
        variadic: Option<u32>,
        body: Val,
        env_id: usize,
    },
    /// Vector of values
    Vector(Vec<Val>),
}

/// Metadata for GC
struct HeapEntry {
    obj: HeapObject,
    marked: bool,
}

pub struct Heap {
    /// hash → entry
    table: HashMap<u64, HeapEntry>,
    /// Monotonic counter for things that must be unique (closures)
    next_unique: u64,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            table: HashMap::new(),
            next_unique: 1,
        }
    }

    // ── Hash helpers ──

    fn mix(a: u64, b: u64, salt: u64) -> u64 {
        // FNV-1a inspired mixing
        let mut h = 0xcbf29ce484222325u64;
        h ^= a;
        h = h.wrapping_mul(0x100000001b3);
        h ^= b;
        h = h.wrapping_mul(0x100000001b3);
        h ^= salt;
        h = h.wrapping_mul(0x100000001b3);
        h & PAYLOAD_MASK
    }

    fn hash_bytes(bytes: &[u8], salt: u64) -> u64 {
        let mut h = 0xcbf29ce484222325u64;
        for &b in bytes {
            h ^= b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        h ^= salt;
        h = h.wrapping_mul(0x100000001b3);
        h & PAYLOAD_MASK
    }

    fn hash_vals(vals: &[Val], salt: u64) -> u64 {
        let mut h = 0xcbf29ce484222325u64;
        for v in vals {
            h ^= v.0;
            h = h.wrapping_mul(0x100000001b3);
        }
        h ^= salt;
        h = h.wrapping_mul(0x100000001b3);
        h & PAYLOAD_MASK
    }

    fn unique_hash(&mut self, salt: u64) -> u64 {
        let id = self.next_unique;
        self.next_unique += 1;
        Self::mix(id, 0, salt)
    }

    // ── Allocation ──

    /// Allocate a cons cell. Hash-consed: identical (car, cdr) → same hash.
    pub fn cons(&mut self, car: Val, cdr: Val) -> Val {
        let hash = Self::mix(car.0, cdr.0, SALT_CONS);
        // Handle (rare) hash collision: check if existing entry is actually equal
        if let Some(entry) = self.table.get(&hash) {
            if let HeapObject::Cons(a, b) = &entry.obj {
                if *a == car && *b == cdr {
                    return Val::heap_ref(hash);
                }
            }
            // Collision with different value — linear probe
            return self.cons_probe(car, cdr, hash);
        }
        self.table.insert(
            hash,
            HeapEntry {
                obj: HeapObject::Cons(car, cdr),
                marked: false,
            },
        );
        Val::heap_ref(hash)
    }

    fn cons_probe(&mut self, car: Val, cdr: Val, base: u64) -> Val {
        for i in 1..1000 {
            let h = (base.wrapping_add(i)) & PAYLOAD_MASK;
            match self.table.get(&h) {
                Some(entry) => {
                    if let HeapObject::Cons(a, b) = &entry.obj {
                        if *a == car && *b == cdr {
                            return Val::heap_ref(h);
                        }
                    }
                    // occupied by something else, keep probing
                }
                None => {
                    self.table.insert(
                        h,
                        HeapEntry {
                            obj: HeapObject::Cons(car, cdr),
                            marked: false,
                        },
                    );
                    return Val::heap_ref(h);
                }
            }
        }
        panic!("heap: too many hash collisions for cons");
    }

    /// Allocate a string. Hash-consed by content.
    pub fn alloc_string(&mut self, s: &str) -> Val {
        let hash = Self::hash_bytes(s.as_bytes(), SALT_STRING);
        if let Some(entry) = self.table.get(&hash) {
            if let HeapObject::Str(existing) = &entry.obj {
                if existing == s {
                    return Val::heap_ref(hash);
                }
            }
            return self.string_probe(s, hash);
        }
        self.table.insert(
            hash,
            HeapEntry {
                obj: HeapObject::Str(s.to_string()),
                marked: false,
            },
        );
        Val::heap_ref(hash)
    }

    fn string_probe(&mut self, s: &str, base: u64) -> Val {
        for i in 1..1000 {
            let h = (base.wrapping_add(i)) & PAYLOAD_MASK;
            match self.table.get(&h) {
                Some(entry) => {
                    if let HeapObject::Str(existing) = &entry.obj {
                        if existing == s {
                            return Val::heap_ref(h);
                        }
                    }
                }
                None => {
                    self.table.insert(
                        h,
                        HeapEntry {
                            obj: HeapObject::Str(s.to_string()),
                            marked: false,
                        },
                    );
                    return Val::heap_ref(h);
                }
            }
        }
        panic!("heap: too many hash collisions for string");
    }

    /// Allocate a closure. Not hash-consed (each closure captures a unique env).
    pub fn alloc_closure(
        &mut self,
        params: Vec<u32>,
        variadic: Option<u32>,
        body: Val,
        env_id: usize,
    ) -> Val {
        let hash = self.unique_hash(SALT_CLOSURE);
        self.table.insert(
            hash,
            HeapEntry {
                obj: HeapObject::Closure {
                    params,
                    variadic,
                    body,
                    env_id,
                },
                marked: false,
            },
        );
        Val::heap_ref(hash)
    }

    /// Allocate a vector. Hash-consed by elements.
    pub fn alloc_vector(&mut self, elems: Vec<Val>) -> Val {
        let hash = Self::hash_vals(&elems, SALT_VECTOR);
        if let Some(entry) = self.table.get(&hash) {
            if let HeapObject::Vector(existing) = &entry.obj {
                if *existing == elems {
                    return Val::heap_ref(hash);
                }
            }
            // collision — use unique to avoid complexity
            let h2 = self.unique_hash(SALT_VECTOR);
            self.table.insert(
                h2,
                HeapEntry {
                    obj: HeapObject::Vector(elems),
                    marked: false,
                },
            );
            return Val::heap_ref(h2);
        }
        self.table.insert(
            hash,
            HeapEntry {
                obj: HeapObject::Vector(elems),
                marked: false,
            },
        );
        Val::heap_ref(hash)
    }

    // ── Access ──

    pub fn get(&self, hash: u64) -> Option<&HeapObject> {
        self.table.get(&hash).map(|e| &e.obj)
    }

    pub fn car(&self, val: Val) -> Option<Val> {
        let h = val.as_heap_ref()?;
        match self.get(h)? {
            HeapObject::Cons(a, _) => Some(*a),
            _ => None,
        }
    }

    pub fn cdr(&self, val: Val) -> Option<Val> {
        let h = val.as_heap_ref()?;
        match self.get(h)? {
            HeapObject::Cons(_, b) => Some(*b),
            _ => None,
        }
    }

    pub fn get_string(&self, val: Val) -> Option<&str> {
        let h = val.as_heap_ref()?;
        match self.get(h)? {
            HeapObject::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_closure(&self, val: Val) -> Option<(&[u32], Option<u32>, Val, usize)> {
        let h = val.as_heap_ref()?;
        match self.get(h)? {
            HeapObject::Closure {
                params,
                variadic,
                body,
                env_id,
            } => Some((params, *variadic, *body, *env_id)),
            _ => None,
        }
    }

    pub fn get_vector(&self, val: Val) -> Option<&[Val]> {
        let h = val.as_heap_ref()?;
        match self.get(h)? {
            HeapObject::Vector(v) => Some(v),
            _ => None,
        }
    }

    /// Is this heap ref a cons cell?
    pub fn is_cons(&self, val: Val) -> bool {
        if let Some(h) = val.as_heap_ref() {
            matches!(self.get(h), Some(HeapObject::Cons(_, _)))
        } else {
            false
        }
    }

    pub fn is_string(&self, val: Val) -> bool {
        if let Some(h) = val.as_heap_ref() {
            matches!(self.get(h), Some(HeapObject::Str(_)))
        } else {
            false
        }
    }

    pub fn is_closure(&self, val: Val) -> bool {
        if let Some(h) = val.as_heap_ref() {
            matches!(self.get(h), Some(HeapObject::Closure { .. }))
        } else {
            false
        }
    }

    pub fn is_vector(&self, val: Val) -> bool {
        if let Some(h) = val.as_heap_ref() {
            matches!(self.get(h), Some(HeapObject::Vector(_)))
        } else {
            false
        }
    }

    // ── Garbage Collection ──

    /// Mark phase: recursively mark a value and everything it references.
    fn mark_val(&mut self, val: Val) {
        if let Some(h) = val.as_heap_ref() {
            if let Some(entry) = self.table.get_mut(&h) {
                if entry.marked {
                    return;
                }
                entry.marked = true;
                // Now we need to recursively mark children, but we can't borrow
                // mutably and immutably at the same time. Collect children first.
                let children: Vec<Val> = match &entry.obj {
                    HeapObject::Cons(a, b) => vec![*a, *b],
                    HeapObject::Str(_) => vec![],
                    HeapObject::Closure { body, .. } => vec![*body],
                    HeapObject::Vector(v) => v.clone(),
                };
                for child in children {
                    self.mark_val(child);
                }
            }
        }
    }

    /// Run mark-and-sweep GC. `roots` are the live Val values from environments, stacks, etc.
    pub fn gc(&mut self, roots: &[Val]) -> usize {
        // Clear all marks
        for entry in self.table.values_mut() {
            entry.marked = false;
        }
        // Mark from roots
        for &root in roots {
            self.mark_val(root);
        }
        // Sweep
        let before = self.table.len();
        self.table.retain(|_, entry| entry.marked);
        before - self.table.len()
    }

    /// Number of objects on heap
    pub fn len(&self) -> usize {
        self.table.len()
    }

    /// Build a Scheme-style proper list from a slice of Vals.
    pub fn list(&mut self, vals: &[Val]) -> Val {
        let mut result = Val::nil();
        for v in vals.iter().rev() {
            result = self.cons(*v, result);
        }
        result
    }

    /// Convert a proper list to a Vec. Returns None if not a proper list.
    pub fn list_to_vec(&self, mut val: Val) -> Option<Vec<Val>> {
        let mut result = Vec::new();
        while !val.is_nil() {
            let h = val.as_heap_ref()?;
            match self.get(h)? {
                HeapObject::Cons(car, cdr) => {
                    result.push(*car);
                    val = *cdr;
                }
                _ => return None,
            }
        }
        Some(result)
    }
}
