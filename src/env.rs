///! Environments for Hashlisp.
///!
///! An environment is a hash-consed association list (alist) stored on the heap.
///! Each binding is a cons cell `(symbol . value)`, and the environment is a
///! chain: `((sym1 . val1) (sym2 . val2) ... . parent-env)`.
///!
///! Because environments are built from cons cells on the hash-consed heap,
///! two environments with identical bindings are automatically shared (eq?).
///! This enables closures themselves to be hash-consed.

use crate::heap::Heap;
use crate::value::Val;

/// Look up a symbol in a hash-consed alist environment.
/// Walks the list until it finds a matching symbol binding.
pub fn env_get(heap: &Heap, env: Val, sym: u32) -> Option<Val> {
    let sym_val = Val::symbol(sym);
    let mut current = env;
    while !current.is_nil() {
        let pair = heap.car(current)?;
        let key = heap.car(pair)?;
        if key == sym_val {
            return heap.cdr(pair);
        }
        current = heap.cdr(current)?;
    }
    None
}

/// Define (or shadow) a binding in an environment.
/// Returns a new environment with the binding prepended.
pub fn env_define(heap: &mut Heap, env: Val, sym: u32, val: Val) -> Val {
    let sym_val = Val::symbol(sym);
    let binding = heap.cons(sym_val, val);
    heap.cons(binding, env)
}

