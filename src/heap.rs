///! Hash-consed heap.
///!
///! Every compound value (cons, string, closure, vector) lives on the heap,
///! keyed by a 61-bit hash derived from its contents.  Before allocating, we
///! compute the hash and check if an identical value already exists.  If so we
///! return the existing hash — this is "hash-consing".
///!
///! Hash computation:
///!   cons(a, b)  → mix(a.0, b.0, SALT_CONS)
///!   string(s)   → fnv1a(bytes(s)) ⊕ SALT_STRING
///!   closure     → mix of param hashes, body hash, env hash ⊕ SALT_CLOSURE
///!   vector      → mix of element hashes ⊕ SALT_VECTOR
///!
///! Closures capture their environment as a hash-consed alist (a Val),
///! so structurally identical closures are automatically shared.
///!
///! Garbage collection: mark-and-sweep from a set of root Val's.

use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};

use crate::value::{Val, PAYLOAD_MASK};

/// A hasher that passes through a pre-hashed u64 unchanged.
/// Keys in our heap table are already high-quality FNV hashes,
/// so re-hashing them is redundant.
#[derive(Default)]
struct IdentityHasher(u64);

impl Hasher for IdentityHasher {
    fn write(&mut self, _bytes: &[u8]) {
        unreachable!("IdentityHasher only supports u64 keys");
    }
    fn write_u64(&mut self, n: u64) {
        self.0 = n;
    }
    fn finish(&self) -> u64 {
        self.0
    }
}

type PreHashedMap<V> = HashMap<u64, V, BuildHasherDefault<IdentityHasher>>;

// ── Salts for different heap object kinds ──
const SALT_CONS: u64 = 0xBEEF_CAFE_1234;
const SALT_STRING: u64 = 0xDEAD_F00D_5678;
const SALT_CLOSURE: u64 = 0xC105_ED00_9ABC;
const SALT_VECTOR: u64 = 0xBADC_0FFE_E135;

/// The kinds of objects stored on the hash-consed heap.
#[derive(Debug, Clone)]
pub enum HeapObject {
    /// (car, cdr)
    Cons(Val, Val),
    /// Immutable string
    Str(String),
    /// Closure: (param_names as symbol ids, body as Val, captured env as hash-consed alist)
    Closure {
        params: Vec<u32>,
        variadic: Option<u32>,
        body: Val,
        env: Val,
        /// If Some, this closure binds itself under this name in every call
        /// (used by named let for recursion without mutating the env).
        self_name: Option<u32>,
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
    /// hash → entry (uses identity hasher since keys are already hashed)
    table: PreHashedMap<HeapEntry>,
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            table: PreHashedMap::default(),
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

    /// Allocate a closure. Hash-consed: identical (params, variadic, body, env) → same hash.
    pub fn alloc_closure(
        &mut self,
        params: Vec<u32>,
        variadic: Option<u32>,
        body: Val,
        env: Val,
        self_name: Option<u32>,
    ) -> Val {
        // Build a deterministic hash from all closure fields
        let mut h = 0xcbf29ce484222325u64;
        for &p in &params {
            h ^= p as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        // Encode variadic presence + value
        h ^= match variadic {
            Some(v) => (v as u64) | (1u64 << 32),
            None => 0,
        };
        h = h.wrapping_mul(0x100000001b3);
        h ^= body.0;
        h = h.wrapping_mul(0x100000001b3);
        h ^= env.0;
        h = h.wrapping_mul(0x100000001b3);
        // Encode self_name
        h ^= match self_name {
            Some(s) => (s as u64) | (1u64 << 33),
            None => 0,
        };
        h = h.wrapping_mul(0x100000001b3);
        h ^= SALT_CLOSURE;
        h = h.wrapping_mul(0x100000001b3);
        let hash = h & PAYLOAD_MASK;

        if let Some(entry) = self.table.get(&hash) {
            if let HeapObject::Closure {
                params: ref ep,
                variadic: ev,
                body: eb,
                env: ee,
                self_name: esn,
            } = entry.obj
            {
                if *ep == params && ev == variadic && eb == body && ee == env && esn == self_name {
                    return Val::heap_ref(hash);
                }
            }
            // Collision — linear probe
            return self.closure_probe(params, variadic, body, env, self_name, hash);
        }
        self.table.insert(
            hash,
            HeapEntry {
                obj: HeapObject::Closure {
                    params,
                    variadic,
                    body,
                    env,
                    self_name,
                },
                marked: false,
            },
        );
        Val::heap_ref(hash)
    }

    fn closure_probe(
        &mut self,
        params: Vec<u32>,
        variadic: Option<u32>,
        body: Val,
        env: Val,
        self_name: Option<u32>,
        base: u64,
    ) -> Val {
        for i in 1..1000 {
            let h = (base.wrapping_add(i)) & PAYLOAD_MASK;
            match self.table.get(&h) {
                Some(entry) => {
                    if let HeapObject::Closure {
                        params: ref ep,
                        variadic: ev,
                        body: eb,
                        env: ee,
                        self_name: esn,
                    } = entry.obj
                    {
                        if *ep == params && ev == variadic && eb == body && ee == env && esn == self_name {
                            return Val::heap_ref(h);
                        }
                    }
                }
                None => {
                    self.table.insert(
                        h,
                        HeapEntry {
                            obj: HeapObject::Closure {
                                params,
                                variadic,
                                body,
                                env,
                                self_name,
                            },
                            marked: false,
                        },
                    );
                    return Val::heap_ref(h);
                }
            }
        }
        panic!("heap: too many hash collisions for closure");
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
            // collision — linear probe
            return self.vector_probe(elems, hash);
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

    fn vector_probe(&mut self, elems: Vec<Val>, base: u64) -> Val {
        for i in 1..1000 {
            let h = (base.wrapping_add(i)) & PAYLOAD_MASK;
            match self.table.get(&h) {
                Some(entry) => {
                    if let HeapObject::Vector(existing) = &entry.obj {
                        if *existing == elems {
                            return Val::heap_ref(h);
                        }
                    }
                }
                None => {
                    self.table.insert(
                        h,
                        HeapEntry {
                            obj: HeapObject::Vector(elems),
                            marked: false,
                        },
                    );
                    return Val::heap_ref(h);
                }
            }
        }
        panic!("heap: too many hash collisions for vector");
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

    pub fn get_closure(&self, val: Val) -> Option<(&[u32], Option<u32>, Val, Val, Option<u32>)> {
        let h = val.as_heap_ref()?;
        match self.get(h)? {
            HeapObject::Closure {
                params,
                variadic,
                body,
                env,
                self_name,
            } => Some((params, *variadic, *body, *env, *self_name)),
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
                    HeapObject::Closure { body, env, .. } => vec![*body, *env],
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
