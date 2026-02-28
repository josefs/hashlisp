///! Pretty-printer for Hashlisp values.

use crate::heap::{Heap, HeapObject};
use crate::symbol::SymbolTable;
use crate::value::Val;

/// Print a value to a string (with quoting for `write` semantics).
pub fn print_val(val: Val, heap: &Heap, syms: &SymbolTable) -> String {
    let mut buf = String::new();
    write_val(val, heap, syms, &mut buf, true);
    buf
}

/// Display a value (no quoting around strings).
#[allow(dead_code)]
pub fn display_val(val: Val, heap: &Heap, syms: &SymbolTable) -> String {
    let mut buf = String::new();
    write_val(val, heap, syms, &mut buf, false);
    buf
}

fn write_val(val: Val, heap: &Heap, syms: &SymbolTable, buf: &mut String, quoting: bool) {
    if let Some(f) = val.as_float() {
        buf.push_str(&format!("{f}"));
        return;
    }
    if let Some(i) = val.as_int() {
        buf.push_str(&i.to_string());
        return;
    }
    if let Some(b) = val.as_bool() {
        buf.push_str(if b { "#t" } else { "#f" });
        return;
    }
    if val.is_nil() {
        buf.push_str("()");
        return;
    }
    if val.is_void() {
        return; // void is silent
    }
    if let Some(c) = val.as_char() {
        if quoting {
            match c {
                ' ' => buf.push_str("#\\space"),
                '\n' => buf.push_str("#\\newline"),
                '\t' => buf.push_str("#\\tab"),
                '\r' => buf.push_str("#\\return"),
                _ => {
                    buf.push_str("#\\");
                    buf.push(c);
                }
            }
        } else {
            buf.push(c);
        }
        return;
    }
    if let Some(sym_id) = val.as_symbol() {
        buf.push_str(syms.name(sym_id));
        return;
    }
    if let Some(_) = val.as_builtin() {
        buf.push_str("#<builtin>");
        return;
    }

    // Heap objects
    if let Some(h) = val.as_heap_ref() {
        match heap.get(h) {
            Some(HeapObject::Cons(_, _)) => {
                write_list(val, heap, syms, buf, quoting);
            }
            Some(HeapObject::Str(s)) => {
                if quoting {
                    buf.push('"');
                    for c in s.chars() {
                        match c {
                            '"' => buf.push_str("\\\""),
                            '\\' => buf.push_str("\\\\"),
                            '\n' => buf.push_str("\\n"),
                            '\t' => buf.push_str("\\t"),
                            _ => buf.push(c),
                        }
                    }
                    buf.push('"');
                } else {
                    buf.push_str(s);
                }
            }
            Some(HeapObject::Closure { .. }) => {
                buf.push_str("#<closure>");
            }
            Some(HeapObject::Vector(v)) => {
                let v = v.clone(); // need to clone to avoid borrow issues
                buf.push_str("#(");
                for (i, elem) in v.iter().enumerate() {
                    if i > 0 {
                        buf.push(' ');
                    }
                    write_val(*elem, heap, syms, buf, quoting);
                }
                buf.push(')');
            }
            None => {
                buf.push_str(&format!("#<dead-ref 0x{h:012x}>"));
            }
        }
        return;
    }

    buf.push_str(&format!("#<unknown 0x{:016x}>", val.0));
}

fn write_list(val: Val, heap: &Heap, syms: &SymbolTable, buf: &mut String, quoting: bool) {
    // Check for (quote x) → 'x
    if let Some(car) = heap.car(val) {
        if let Some(sym_id) = car.as_symbol() {
            if syms.name(sym_id) == "quote" {
                if let Some(cdr) = heap.cdr(val) {
                    if let Some(datum) = heap.car(cdr) {
                        if heap.cdr(cdr).map_or(false, |v| v.is_nil()) {
                            buf.push('\'');
                            write_val(datum, heap, syms, buf, quoting);
                            return;
                        }
                    }
                }
            }
        }
    }

    buf.push('(');
    let mut current = val;
    let mut first = true;
    loop {
        if current.is_nil() {
            break;
        }
        if !first {
            buf.push(' ');
        }
        first = false;

        if let Some(h) = current.as_heap_ref() {
            match heap.get(h) {
                Some(HeapObject::Cons(car, cdr)) => {
                    write_val(*car, heap, syms, buf, quoting);
                    if !cdr.is_nil() && !cdr.is_heap() {
                        // Improper list
                        buf.push_str(" . ");
                        write_val(*cdr, heap, syms, buf, quoting);
                        break;
                    }
                    if cdr.is_heap() && !heap.is_cons(*cdr) {
                        // Also improper
                        buf.push_str(" . ");
                        write_val(*cdr, heap, syms, buf, quoting);
                        break;
                    }
                    current = *cdr;
                }
                _ => {
                    write_val(current, heap, syms, buf, quoting);
                    break;
                }
            }
        } else {
            write_val(current, heap, syms, buf, quoting);
            break;
        }
    }
    buf.push(')');
}
