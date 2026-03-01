///! Evaluator for Hashlisp — a tree-walking interpreter.
///!
///! Supports: define, lambda, if, cond, let, let*, letrec, begin, quote,
///!           set!, and, or, do (named let).
///! Tail-call optimization via an explicit trampoline loop.

use std::collections::HashMap;
use std::io::Write;

use crate::env::EnvStore;
use crate::heap::Heap;
use crate::parser;
use crate::symbol::SymbolTable;
use crate::value::Val;
use crate::printer;

/// The result of evaluation: either a final value or a tail-call to trampoline.
enum Trampoline {
    Done(Val),
    TailCall { expr: Val, env_id: usize },
}

#[allow(dead_code)]
pub struct Evaluator {
    pub heap: Heap,
    pub syms: SymbolTable,
    pub envs: EnvStore,
    pub global_env: usize,
    gc_threshold: usize,
    alloc_since_gc: usize,
    /// Extra GC roots — e.g. parsed but not-yet-evaluated expressions.
    pub extra_roots: Vec<Val>,
    /// Shadow stack — protects intermediate values on the Rust call stack from GC.
    shadow_stack: Vec<Val>,
    /// Macro table: symbol id → transformer closure Val
    macros: HashMap<u32, Val>,
    /// Memo tables: closure Val → (args-key Val → result Val)
    memo_tables: HashMap<Val, HashMap<Val, Val>>,
    /// Counter for gensym
    gensym_counter: u64,
    // well-known symbol ids (cached)
    sym_quote: u32,
    sym_if: u32,
    sym_cond: u32,
    sym_define: u32,
    sym_lambda: u32,
    sym_let: u32,
    sym_letstar: u32,
    sym_letrec: u32,
    sym_begin: u32,
    sym_set: u32,
    sym_and: u32,
    sym_or: u32,
    sym_else: u32,
    sym_do: u32,
    sym_when: u32,
    sym_unless: u32,
    sym_apply: u32,
    sym_quasiquote: u32,
    sym_unquote: u32,
    sym_unquote_splicing: u32,
    sym_define_macro: u32,
    sym_define_memo: u32,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut syms = SymbolTable::new();
        let mut envs = EnvStore::new();
        let heap = Heap::new();
        let global_env = envs.new_top_level();

        let sym_quote = syms.intern("quote");
        let sym_if = syms.intern("if");
        let sym_cond = syms.intern("cond");
        let sym_define = syms.intern("define");
        let sym_lambda = syms.intern("lambda");
        let sym_let = syms.intern("let");
        let sym_letstar = syms.intern("let*");
        let sym_letrec = syms.intern("letrec");
        let sym_begin = syms.intern("begin");
        let sym_set = syms.intern("set!");
        let sym_and = syms.intern("and");
        let sym_or = syms.intern("or");
        let sym_else = syms.intern("else");
        let sym_do = syms.intern("do");
        let sym_when = syms.intern("when");
        let sym_unless = syms.intern("unless");
        let sym_apply = syms.intern("apply");
        let sym_quasiquote = syms.intern("quasiquote");
        let sym_unquote = syms.intern("unquote");
        let sym_unquote_splicing = syms.intern("unquote-splicing");
        let sym_define_macro = syms.intern("define-macro");
        let sym_define_memo = syms.intern("define-memo");

        let mut eval = Evaluator {
            heap,
            syms,
            envs,
            global_env,
            gc_threshold: 10000,
            alloc_since_gc: 0,
            extra_roots: Vec::new(),
            shadow_stack: Vec::new(),
            macros: HashMap::new(),
            memo_tables: HashMap::new(),
            gensym_counter: 0,
            sym_quote,
            sym_if,
            sym_cond,
            sym_define,
            sym_lambda,
            sym_let,
            sym_letstar,
            sym_letrec,
            sym_begin,
            sym_set,
            sym_and,
            sym_or,
            sym_else,
            sym_do,
            sym_when,
            sym_unless,
            sym_apply,
            sym_quasiquote,
            sym_unquote,
            sym_unquote_splicing,
            sym_define_macro,
            sym_define_memo,
        };

        eval.register_builtins();
        eval
    }

    fn maybe_gc(&mut self) {
        self.alloc_since_gc += 1;
        if self.alloc_since_gc >= self.gc_threshold {
            let mut roots = self.envs.all_values();
            roots.extend_from_slice(&self.extra_roots);
            roots.extend_from_slice(&self.shadow_stack);
            roots.extend(self.macros.values().copied());
            for table in self.memo_tables.values() {
                for (k, v) in table.iter() {
                    roots.push(*k);
                    roots.push(*v);
                }
            }
            let swept = self.heap.gc(&roots);
            if swept > 0 {
                // Adaptive threshold
                self.gc_threshold = (self.heap.len() * 2).max(1000);
            }
            self.alloc_since_gc = 0;
        }
    }

    // ── Public eval entry ──

    pub fn eval(&mut self, expr: Val) -> Result<Val, String> {
        self.eval_in(expr, self.global_env)
    }

    pub fn eval_in(&mut self, expr: Val, env_id: usize) -> Result<Val, String> {
        // Trampoline loop for TCO
        let mut current_expr = expr;
        let mut current_env = env_id;
        loop {
            match self.eval_inner(current_expr, current_env)? {
                Trampoline::Done(val) => return Ok(val),
                Trampoline::TailCall { expr, env_id } => {
                    current_expr = expr;
                    current_env = env_id;
                }
            }
        }
    }

    fn eval_inner(&mut self, expr: Val, env_id: usize) -> Result<Trampoline, String> {
        self.maybe_gc();

        // Protect the current expression (and all its sub-trees) from GC
        // while we decompose and evaluate it.
        self.shadow_stack.push(expr);

        let result = self.eval_inner_impl(expr, env_id);

        self.shadow_stack.pop();
        result
    }

    fn eval_inner_impl(&mut self, expr: Val, env_id: usize) -> Result<Trampoline, String> {

        // ── Self-evaluating ──
        if let Some(_) = expr.as_int() {
            return Ok(Trampoline::Done(expr));
        }
        if let Some(_) = expr.as_bool() {
            return Ok(Trampoline::Done(expr));
        }
        if let Some(_) = expr.as_char() {
            return Ok(Trampoline::Done(expr));
        }
        if expr.is_nil() {
            return Ok(Trampoline::Done(expr));
        }
        if expr.is_void() {
            return Ok(Trampoline::Done(expr));
        }
        if let Some(_) = expr.as_builtin() {
            return Ok(Trampoline::Done(expr));
        }

        // ── String literal (heap) ──
        if expr.is_heap() && self.heap.is_string(expr) {
            return Ok(Trampoline::Done(expr));
        }

        // ── Symbol → lookup ──
        if let Some(sym_id) = expr.as_symbol() {
            return match self.envs.get(env_id, sym_id) {
                Some(val) => Ok(Trampoline::Done(val)),
                None => Err(format!("unbound variable: {}", self.syms.name(sym_id))),
            };
        }

        // ── List (application or special form) ──
        if expr.is_heap() && self.heap.is_cons(expr) {
            let car = self.heap.car(expr).unwrap();
            let cdr = self.heap.cdr(expr).unwrap();

            // Special forms (check if car is a known symbol)
            if let Some(sym_id) = car.as_symbol() {
                if sym_id == self.sym_quote {
                    return self.eval_quote(cdr);
                }
                if sym_id == self.sym_if {
                    return self.eval_if(cdr, env_id);
                }
                if sym_id == self.sym_cond {
                    return self.eval_cond(cdr, env_id);
                }
                if sym_id == self.sym_define {
                    return self.eval_define(cdr, env_id);
                }
                if sym_id == self.sym_lambda {
                    return self.eval_lambda(cdr, env_id);
                }
                if sym_id == self.sym_let {
                    return self.eval_let(cdr, env_id);
                }
                if sym_id == self.sym_letstar {
                    return self.eval_let_star(cdr, env_id);
                }
                if sym_id == self.sym_letrec {
                    return self.eval_letrec(cdr, env_id);
                }
                if sym_id == self.sym_begin {
                    return self.eval_begin(cdr, env_id);
                }
                if sym_id == self.sym_set {
                    return self.eval_set(cdr, env_id);
                }
                if sym_id == self.sym_and {
                    return self.eval_and(cdr, env_id);
                }
                if sym_id == self.sym_or {
                    return self.eval_or(cdr, env_id);
                }
                if sym_id == self.sym_when {
                    return self.eval_when(cdr, env_id);
                }
                if sym_id == self.sym_unless {
                    return self.eval_unless(cdr, env_id);
                }
                if sym_id == self.sym_quasiquote {
                    let template = self.heap.car(cdr).ok_or("quasiquote: expected argument")?;
                    let result = self.eval_quasiquote(template, env_id)?;
                    return Ok(Trampoline::Done(result));
                }
                if sym_id == self.sym_define_macro {
                    return self.eval_define_macro(cdr, env_id);
                }
                if sym_id == self.sym_define_memo {
                    return self.eval_define_memo(cdr, env_id);
                }
                // Check for macro invocation
                if self.macros.contains_key(&sym_id) {
                    return self.eval_macro_call(sym_id, cdr, env_id);
                }
            }

            // Regular application
            return self.eval_apply(car, cdr, env_id);
        }

        // Vectors and closures are also self-evaluating
        if expr.is_heap() {
            return Ok(Trampoline::Done(expr));
        }

        Err(format!("cannot evaluate: {:?}", expr))
    }

    // ── Special forms ──

    fn eval_quote(&self, args: Val) -> Result<Trampoline, String> {
        let datum = self.heap.car(args).ok_or("quote: expected argument")?;
        Ok(Trampoline::Done(datum))
    }

    fn eval_if(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let condition = self.heap.car(args).ok_or("if: expected condition")?;
        let rest = self.heap.cdr(args).ok_or("if: expected consequent")?;
        let consequent = self.heap.car(rest).ok_or("if: expected consequent")?;
        let alt_pair = self.heap.cdr(rest);

        let cond_val = self.eval_in(condition, env_id)?;
        if cond_val.is_truthy() {
            Ok(Trampoline::TailCall {
                expr: consequent,
                env_id,
            })
        } else if let Some(alt_rest) = alt_pair {
            if alt_rest.is_nil() {
                Ok(Trampoline::Done(Val::void()))
            } else {
                let alternative = self.heap.car(alt_rest).ok_or("if: bad alternative")?;
                Ok(Trampoline::TailCall {
                    expr: alternative,
                    env_id,
                })
            }
        } else {
            Ok(Trampoline::Done(Val::void()))
        }
    }

    fn eval_cond(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let clauses = self.heap.list_to_vec(args).ok_or("cond: expected list of clauses")?;
        for clause in &clauses {
            let parts = self.heap.list_to_vec(*clause).ok_or("cond: bad clause")?;
            if parts.is_empty() {
                return Err("cond: empty clause".into());
            }
            let test = parts[0];
            // (else ...) clause
            if test.as_symbol() == Some(self.sym_else) {
                if parts.len() == 1 {
                    return Ok(Trampoline::Done(Val::void()));
                }
                // Evaluate all body exprs, tail-call the last
                for i in 1..parts.len() - 1 {
                    self.eval_in(parts[i], env_id)?;
                }
                return Ok(Trampoline::TailCall {
                    expr: *parts.last().unwrap(),
                    env_id,
                });
            }
            let test_val = self.eval_in(test, env_id)?;
            if test_val.is_truthy() {
                if parts.len() == 1 {
                    return Ok(Trampoline::Done(test_val));
                }
                for i in 1..parts.len() - 1 {
                    self.eval_in(parts[i], env_id)?;
                }
                return Ok(Trampoline::TailCall {
                    expr: *parts.last().unwrap(),
                    env_id,
                });
            }
        }
        Ok(Trampoline::Done(Val::void()))
    }

    fn eval_define(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let first = self.heap.car(args).ok_or("define: expected name")?;
        let rest = self.heap.cdr(args).ok_or("define: expected value")?;

        // (define x expr)
        if let Some(sym_id) = first.as_symbol() {
            let val_expr = self.heap.car(rest).ok_or("define: expected value")?;
            let val = self.eval_in(val_expr, env_id)?;
            self.envs.define(env_id, sym_id, val);
            return Ok(Trampoline::Done(Val::void()));
        }

        // (define (f args...) body...)  → sugar for (define f (lambda (args...) body...))
        if self.heap.is_cons(first) {
            let name = self.heap.car(first).ok_or("define: expected function name")?;
            let sym_id = name
                .as_symbol()
                .ok_or("define: function name must be a symbol")?;
            let params = self.heap.cdr(first).ok_or("define: expected params")?;
            // Build (lambda (params) body...)
            let lambda_sym = Val::symbol(self.sym_lambda);
            let lambda_args = self.heap.cons(params, rest);
            let lambda_expr = self.heap.cons(lambda_sym, lambda_args);
            let val = self.eval_in(lambda_expr, env_id)?;
            self.envs.define(env_id, sym_id, val);
            return Ok(Trampoline::Done(Val::void()));
        }

        Err("define: bad syntax".into())
    }

    fn eval_lambda(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let params_form = self.heap.car(args).ok_or("lambda: expected params")?;
        let body = self.heap.cdr(args).ok_or("lambda: expected body")?;

        // Parse params - could be a proper list, improper list (variadic), or symbol (all-variadic)
        let (params, variadic) = self.parse_params(params_form)?;

        // Wrap body in begin if multiple expressions
        let body_val = {
            let parts = self.heap.list_to_vec(body);
            match parts {
                Some(ref v) if v.len() == 1 => v[0],
                _ => {
                    let begin_sym = Val::symbol(self.sym_begin);
                    self.heap.cons(begin_sym, body)
                }
            }
        };

        let closure = self.heap.alloc_closure(params, variadic, body_val, env_id);
        Ok(Trampoline::Done(closure))
    }

    fn parse_params(&self, form: Val) -> Result<(Vec<u32>, Option<u32>), String> {
        // Single symbol → variadic: (lambda x body)
        if let Some(sym_id) = form.as_symbol() {
            return Ok((vec![], Some(sym_id)));
        }
        // Nil → no params: (lambda () body)
        if form.is_nil() {
            return Ok((vec![], None));
        }
        // Walk list
        let mut params = Vec::new();
        let mut current = form;
        loop {
            if current.is_nil() {
                return Ok((params, None));
            }
            if let Some(sym_id) = current.as_symbol() {
                // Improper list → rest param
                return Ok((params, Some(sym_id)));
            }
            let car = self
                .heap
                .car(current)
                .ok_or("lambda: bad parameter list")?;
            let sym_id = car
                .as_symbol()
                .ok_or("lambda: parameter must be a symbol")?;
            params.push(sym_id);
            current = self
                .heap
                .cdr(current)
                .ok_or("lambda: bad parameter list")?;
        }
    }

    fn eval_let(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let first = self.heap.car(args).ok_or("let: expected bindings")?;

        // Named let: (let name ((var init) ...) body ...)
        if let Some(name_sym) = first.as_symbol() {
            let rest = self.heap.cdr(args).ok_or("named let: expected bindings")?;
            let bindings_form = self.heap.car(rest).ok_or("named let: expected bindings")?;
            let body = self.heap.cdr(rest).ok_or("named let: expected body")?;

            let bindings = self
                .heap
                .list_to_vec(bindings_form)
                .ok_or("named let: bindings must be a list")?;

            let mut param_names = Vec::new();
            let mut init_vals = Vec::new();

            for b in &bindings {
                let pair = self.heap.list_to_vec(*b).ok_or("named let: bad binding")?;
                if pair.len() != 2 {
                    return Err("named let: binding must be (var init)".into());
                }
                let sym_id = pair[0]
                    .as_symbol()
                    .ok_or("named let: variable must be a symbol")?;
                let init = self.eval_in(pair[1], env_id)?;
                param_names.push(sym_id);
                init_vals.push(init);
            }

            // Wrap body in begin
            let body_val = {
                let parts = self.heap.list_to_vec(body);
                match parts {
                    Some(ref v) if v.len() == 1 => v[0],
                    _ => {
                        let begin_sym = Val::symbol(self.sym_begin);
                        self.heap.cons(begin_sym, body)
                    }
                }
            };

            // Create closure and bind it in its own env
            let loop_env = self.envs.new_child(env_id);
            let closure =
                self.heap
                    .alloc_closure(param_names.clone(), None, body_val, loop_env);
            self.envs.define(loop_env, name_sym, closure);

            // Apply closure with initial values
            let call_env = self.envs.new_child(loop_env);
            for (i, &sym) in param_names.iter().enumerate() {
                self.envs.define(call_env, sym, init_vals[i]);
            }
            return Ok(Trampoline::TailCall {
                expr: body_val,
                env_id: call_env,
            });
        }

        let bindings_form = first;
        let body = self.heap.cdr(args).ok_or("let: expected body")?;

        let bindings = self
            .heap
            .list_to_vec(bindings_form)
            .ok_or("let: bindings must be a list")?;

        let child_env = self.envs.new_child(env_id);

        for b in &bindings {
            let pair = self.heap.list_to_vec(*b).ok_or("let: bad binding")?;
            if pair.len() != 2 {
                return Err("let: binding must be (var init)".into());
            }
            let sym_id = pair[0]
                .as_symbol()
                .ok_or("let: variable must be a symbol")?;
            let val = self.eval_in(pair[1], env_id)?;
            self.envs.define(child_env, sym_id, val);
        }

        // Eval body forms
        let body_parts = self.heap.list_to_vec(body).ok_or("let: body must be a list")?;
        if body_parts.is_empty() {
            return Err("let: empty body".into());
        }
        for i in 0..body_parts.len() - 1 {
            self.eval_in(body_parts[i], child_env)?;
        }
        Ok(Trampoline::TailCall {
            expr: *body_parts.last().unwrap(),
            env_id: child_env,
        })
    }

    fn eval_let_star(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let bindings_form = self.heap.car(args).ok_or("let*: expected bindings")?;
        let body = self.heap.cdr(args).ok_or("let*: expected body")?;

        let bindings = self
            .heap
            .list_to_vec(bindings_form)
            .ok_or("let*: bindings must be a list")?;

        let child_env = self.envs.new_child(env_id);

        for b in &bindings {
            let pair = self.heap.list_to_vec(*b).ok_or("let*: bad binding")?;
            if pair.len() != 2 {
                return Err("let*: binding must be (var init)".into());
            }
            let sym_id = pair[0]
                .as_symbol()
                .ok_or("let*: variable must be a symbol")?;
            let val = self.eval_in(pair[1], child_env)?;
            self.envs.define(child_env, sym_id, val);
        }

        let body_parts = self
            .heap
            .list_to_vec(body)
            .ok_or("let*: body must be a list")?;
        if body_parts.is_empty() {
            return Err("let*: empty body".into());
        }
        for i in 0..body_parts.len() - 1 {
            self.eval_in(body_parts[i], child_env)?;
        }
        Ok(Trampoline::TailCall {
            expr: *body_parts.last().unwrap(),
            env_id: child_env,
        })
    }

    fn eval_letrec(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let bindings_form = self.heap.car(args).ok_or("letrec: expected bindings")?;
        let body = self.heap.cdr(args).ok_or("letrec: expected body")?;

        let bindings = self
            .heap
            .list_to_vec(bindings_form)
            .ok_or("letrec: bindings must be a list")?;

        let child_env = self.envs.new_child(env_id);

        // First, define all vars as void
        let mut syms_list = Vec::new();
        let mut exprs_list = Vec::new();
        for b in &bindings {
            let pair = self.heap.list_to_vec(*b).ok_or("letrec: bad binding")?;
            if pair.len() != 2 {
                return Err("letrec: binding must be (var init)".into());
            }
            let sym_id = pair[0]
                .as_symbol()
                .ok_or("letrec: variable must be a symbol")?;
            syms_list.push(sym_id);
            exprs_list.push(pair[1]);
            self.envs.define(child_env, sym_id, Val::void());
        }

        // Now evaluate and assign
        for (i, &sym_id) in syms_list.iter().enumerate() {
            let val = self.eval_in(exprs_list[i], child_env)?;
            self.envs.define(child_env, sym_id, val);
        }

        let body_parts = self
            .heap
            .list_to_vec(body)
            .ok_or("letrec: body must be a list")?;
        if body_parts.is_empty() {
            return Err("letrec: empty body".into());
        }
        for i in 0..body_parts.len() - 1 {
            self.eval_in(body_parts[i], child_env)?;
        }
        Ok(Trampoline::TailCall {
            expr: *body_parts.last().unwrap(),
            env_id: child_env,
        })
    }

    fn eval_begin(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let parts = self
            .heap
            .list_to_vec(args)
            .ok_or("begin: expected list of expressions")?;
        if parts.is_empty() {
            return Ok(Trampoline::Done(Val::void()));
        }
        for i in 0..parts.len() - 1 {
            self.eval_in(parts[i], env_id)?;
        }
        Ok(Trampoline::TailCall {
            expr: *parts.last().unwrap(),
            env_id,
        })
    }

    fn eval_set(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let name = self.heap.car(args).ok_or("set!: expected name")?;
        let sym_id = name.as_symbol().ok_or("set!: name must be a symbol")?;
        let val_expr = self
            .heap
            .car(self.heap.cdr(args).ok_or("set!: expected value")?)
            .ok_or("set!: expected value")?;
        let val = self.eval_in(val_expr, env_id)?;
        if !self.envs.set(env_id, sym_id, val) {
            return Err(format!(
                "set!: unbound variable: {}",
                self.syms.name(sym_id)
            ));
        }
        Ok(Trampoline::Done(Val::void()))
    }

    fn eval_and(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        if args.is_nil() {
            return Ok(Trampoline::Done(Val::boolean(true)));
        }
        let parts = self
            .heap
            .list_to_vec(args)
            .ok_or("and: expected list")?;
        for i in 0..parts.len() - 1 {
            let val = self.eval_in(parts[i], env_id)?;
            if !val.is_truthy() {
                return Ok(Trampoline::Done(val));
            }
        }
        Ok(Trampoline::TailCall {
            expr: *parts.last().unwrap(),
            env_id,
        })
    }

    fn eval_or(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        if args.is_nil() {
            return Ok(Trampoline::Done(Val::boolean(false)));
        }
        let parts = self
            .heap
            .list_to_vec(args)
            .ok_or("or: expected list")?;
        for i in 0..parts.len() - 1 {
            let val = self.eval_in(parts[i], env_id)?;
            if val.is_truthy() {
                return Ok(Trampoline::Done(val));
            }
        }
        Ok(Trampoline::TailCall {
            expr: *parts.last().unwrap(),
            env_id,
        })
    }

    fn eval_when(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let condition = self.heap.car(args).ok_or("when: expected condition")?;
        let body = self.heap.cdr(args).ok_or("when: expected body")?;
        let cond_val = self.eval_in(condition, env_id)?;
        if cond_val.is_truthy() {
            self.eval_begin(body, env_id)
        } else {
            Ok(Trampoline::Done(Val::void()))
        }
    }

    fn eval_unless(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        let condition = self.heap.car(args).ok_or("unless: expected condition")?;
        let body = self.heap.cdr(args).ok_or("unless: expected body")?;
        let cond_val = self.eval_in(condition, env_id)?;
        if !cond_val.is_truthy() {
            self.eval_begin(body, env_id)
        } else {
            Ok(Trampoline::Done(Val::void()))
        }
    }

    // ── Quasiquote ──

    fn eval_quasiquote(&mut self, template: Val, env_id: usize) -> Result<Val, String> {
        // If it's not a list/pair, return as-is (like quote)
        if !template.is_heap() || !self.heap.is_cons(template) {
            return Ok(template);
        }

        let car = self.heap.car(template).unwrap();
        let cdr = self.heap.cdr(template).unwrap();

        // (unquote expr) → evaluate expr
        if let Some(sym_id) = car.as_symbol() {
            if sym_id == self.sym_unquote {
                let expr = self.heap.car(cdr).ok_or("unquote: expected argument")?;
                return self.eval_in(expr, env_id);
            }
        }

        // Process list elements, handling unquote-splicing
        let mut elems: Vec<Val> = Vec::new();
        let mut rest = template;
        while rest.is_heap() && self.heap.is_cons(rest) {
            // Check if rest itself is (unquote expr) — handles dotted-pair unquote
            let rest_car = self.heap.car(rest).unwrap();
            if let Some(sym_id) = rest_car.as_symbol() {
                if sym_id == self.sym_unquote {
                    let rest_cdr = self.heap.cdr(rest).unwrap_or(Val::nil());
                    let expr = self.heap.car(rest_cdr).ok_or("unquote: expected argument")?;
                    let val = self.eval_in(expr, env_id)?;
                    // This is the tail — build result with val as cdr
                    rest = val;
                    break;
                }
            }

            let item = rest_car;
            rest = self.heap.cdr(rest).unwrap();

            // Check for (unquote-splicing expr)
            if item.is_heap() && self.heap.is_cons(item) {
                let item_car = self.heap.car(item).unwrap();
                if let Some(sym_id) = item_car.as_symbol() {
                    if sym_id == self.sym_unquote_splicing {
                        let splice_expr = self.heap.car(self.heap.cdr(item).unwrap_or(Val::nil()))
                            .ok_or("unquote-splicing: expected argument")?;
                        let splice_val = self.eval_in(splice_expr, env_id)?;
                        // splice_val should be a list; append its elements
                        let splice_elems = self.heap.list_to_vec(splice_val)
                            .ok_or("unquote-splicing: result must be a list")?;
                        let ss_base = self.shadow_stack.len();
                        for v in &splice_elems {
                            self.shadow_stack.push(*v);
                        }
                        elems.extend(splice_elems);
                        self.shadow_stack.truncate(ss_base);
                        continue;
                    }
                }
            }

            // Recursively process the element
            let processed = self.eval_quasiquote(item, env_id)?;
            self.shadow_stack.push(processed);
            elems.push(processed);
        }

        // Build the result list (with possible dotted tail)
        let tail = if rest.is_nil() {
            Val::nil()
        } else {
            self.eval_quasiquote(rest, env_id)?
        };
        let mut result = tail;
        for v in elems.iter().rev() {
            result = self.heap.cons(*v, result);
        }
        // Pop shadow stack entries for elems
        let to_pop = elems.len();
        let len = self.shadow_stack.len();
        if len >= to_pop {
            self.shadow_stack.truncate(len - to_pop);
        }
        Ok(result)
    }

    // ── Macros ──

    fn eval_define_macro(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        // (define-macro (name params...) body...)
        // or (define-macro name transformer-expr)
        let first = self.heap.car(args).ok_or("define-macro: expected name or (name params...)")?;
        let rest = self.heap.cdr(args).ok_or("define-macro: expected body")?;

        if first.is_heap() && self.heap.is_cons(first) {
            // (define-macro (name params...) body...)
            let name = self.heap.car(first).ok_or("define-macro: expected name")?;
            let name_sym = name.as_symbol().ok_or("define-macro: name must be a symbol")?;
            let params_list = self.heap.cdr(first).ok_or("define-macro: expected params")?;

            // Build a lambda from (params...) body...
            let lambda_sym = Val::symbol(self.sym_lambda);
            let inner = self.heap.cons(params_list, rest);
            let lambda_form = self.heap.cons(lambda_sym, inner);
            let transformer = self.eval_in(lambda_form, env_id)?;
            self.macros.insert(name_sym, transformer);
            Ok(Trampoline::Done(Val::void()))
        } else {
            // (define-macro name transformer-expr)
            let name_sym = first.as_symbol().ok_or("define-macro: name must be a symbol")?;
            let transformer_expr = self.heap.car(rest).ok_or("define-macro: expected transformer")?;
            let transformer = self.eval_in(transformer_expr, env_id)?;
            self.macros.insert(name_sym, transformer);
            Ok(Trampoline::Done(Val::void()))
        }
    }

    // ── Memoized functions ──

    fn eval_define_memo(&mut self, args: Val, env_id: usize) -> Result<Trampoline, String> {
        // (define-memo (name params...) body...)
        let first = self.heap.car(args).ok_or("define-memo: expected (name params...)")?;
        let rest = self.heap.cdr(args).ok_or("define-memo: expected body")?;

        if first.is_heap() && self.heap.is_cons(first) {
            let name = self.heap.car(first).ok_or("define-memo: expected function name")?;
            let sym_id = name.as_symbol().ok_or("define-memo: function name must be a symbol")?;
            let params = self.heap.cdr(first).ok_or("define-memo: expected params")?;

            // Build (lambda (params) body...)
            let lambda_sym = Val::symbol(self.sym_lambda);
            let lambda_args = self.heap.cons(params, rest);
            let lambda_expr = self.heap.cons(lambda_sym, lambda_args);
            let closure = self.eval_in(lambda_expr, env_id)?;

            // Register memo table for this closure
            self.memo_tables.insert(closure, HashMap::new());
            self.envs.define(env_id, sym_id, closure);
            Ok(Trampoline::Done(Val::void()))
        } else {
            Err("define-memo: expected (name params...) form".into())
        }
    }

    fn eval_macro_call(&mut self, macro_sym: u32, args_list: Val, env_id: usize) -> Result<Trampoline, String> {
        let transformer = *self.macros.get(&macro_sym).unwrap();
        // Pass unevaluated arguments to the transformer
        let arg_vals = self.heap.list_to_vec(args_list)
            .ok_or("macro call: arguments must be a proper list")?;
        let expanded = self.apply_func(transformer, arg_vals)?;
        // Evaluate the expanded form
        let expanded_val = match expanded {
            Trampoline::Done(v) => v,
            Trampoline::TailCall { expr, env_id: e } => self.eval_in(expr, e)?,
        };
        // Evaluate the macro-expanded code in the caller's environment (tail position)
        Ok(Trampoline::TailCall { expr: expanded_val, env_id })
    }

    // ── Application ──

    fn eval_apply(
        &mut self,
        func_expr: Val,
        args_list: Val,
        env_id: usize,
    ) -> Result<Trampoline, String> {
        let func = self.eval_in(func_expr, env_id)?;
        self.shadow_stack.push(func);

        // Evaluate arguments
        let arg_exprs = self
            .heap
            .list_to_vec(args_list)
            .ok_or("apply: arguments must be a proper list")?;

        // Check for (apply f args-list) special builtin
        if let Some(sym_id) = func_expr.as_symbol() {
            if sym_id == self.sym_apply {
                self.shadow_stack.pop();
                return self.eval_apply_builtin(&arg_exprs, env_id);
            }
        }

        let ss_base = self.shadow_stack.len();
        let mut arg_vals = Vec::with_capacity(arg_exprs.len());
        for expr in &arg_exprs {
            let v = self.eval_in(*expr, env_id)?;
            self.shadow_stack.push(v);
            arg_vals.push(v);
        }
        self.shadow_stack.truncate(ss_base);
        self.shadow_stack.pop(); // remove func

        self.apply_func(func, arg_vals)
    }

    fn eval_apply_builtin(
        &mut self,
        arg_exprs: &[Val],
        env_id: usize,
    ) -> Result<Trampoline, String> {
        if arg_exprs.len() != 2 {
            return Err("apply: expected (apply func args-list)".into());
        }
        let func = self.eval_in(arg_exprs[0], env_id)?;
        self.shadow_stack.push(func);
        let args_list_val = self.eval_in(arg_exprs[1], env_id)?;
        self.shadow_stack.pop();
        let arg_vals = self
            .heap
            .list_to_vec(args_list_val)
            .ok_or("apply: second argument must be a list")?;
        self.apply_func(func, arg_vals)
    }

    fn apply_func(&mut self, func: Val, arg_vals: Vec<Val>) -> Result<Trampoline, String> {
        // Protect func and args from GC
        let ss_base = self.shadow_stack.len();
        self.shadow_stack.push(func);
        self.shadow_stack.extend_from_slice(&arg_vals);

        let result = self.apply_func_inner(func, arg_vals);

        self.shadow_stack.truncate(ss_base);
        result
    }

    fn apply_func_inner(&mut self, func: Val, arg_vals: Vec<Val>) -> Result<Trampoline, String> {
        // Builtin function
        if let Some(builtin_id) = func.as_builtin() {
            let result = self.call_builtin(builtin_id, &arg_vals)?;
            return Ok(Trampoline::Done(result));
        }

        // Memoized closure: check cache before executing
        if self.memo_tables.contains_key(&func) {
            let key = self.heap.list(&arg_vals);
            if let Some(cached) = self.memo_tables.get(&func).and_then(|t| t.get(&key)).copied() {
                return Ok(Trampoline::Done(cached));
            }
            // Miss — compute the result fully (can't use TailCall, we need the value to cache)
            // Protect key from GC during body evaluation
            self.shadow_stack.push(key);
            let trampoline = self.apply_closure(func, arg_vals)?;
            let result = match trampoline {
                Trampoline::Done(v) => v,
                Trampoline::TailCall { expr, env_id } => self.eval_in(expr, env_id)?,
            };
            self.shadow_stack.pop();
            self.memo_tables.get_mut(&func).unwrap().insert(key, result);
            return Ok(Trampoline::Done(result));
        }

        // Regular closure (non-memoized)
        self.apply_closure(func, arg_vals)
    }

    /// Apply a closure (shared logic for memoized and regular calls)
    fn apply_closure(&mut self, func: Val, arg_vals: Vec<Val>) -> Result<Trampoline, String> {
        if let Some((params, variadic, body, closure_env)) = self.heap.get_closure(func) {
            let params = params.to_vec();
            let call_env = self.envs.new_child(closure_env);

            if let Some(rest_param) = variadic {
                if arg_vals.len() < params.len() {
                    return Err(format!(
                        "wrong number of arguments: expected at least {}, got {}",
                        params.len(),
                        arg_vals.len()
                    ));
                }
                for (i, &sym) in params.iter().enumerate() {
                    self.envs.define(call_env, sym, arg_vals[i]);
                }
                let rest = self.heap.list(&arg_vals[params.len()..]);
                self.envs.define(call_env, rest_param, rest);
            } else {
                if arg_vals.len() != params.len() {
                    return Err(format!(
                        "wrong number of arguments: expected {}, got {}",
                        params.len(),
                        arg_vals.len()
                    ));
                }
                for (i, &sym) in params.iter().enumerate() {
                    self.envs.define(call_env, sym, arg_vals[i]);
                }
            }

            return Ok(Trampoline::TailCall {
                expr: body,
                env_id: call_env,
            });
        }

        Err(format!(
            "not a closure: {}",
            printer::print_val(func, &self.heap, &self.syms)
        ))
    }

    // ── Builtins ──

    fn register_builtins(&mut self) {
        let builtins: Vec<(&str, u32)> = vec![
            ("+", 0),
            ("-", 1),
            ("*", 2),
            ("/", 3),
            ("=", 4),
            ("<", 5),
            (">", 6),
            ("<=", 7),
            (">=", 8),
            ("cons", 9),
            ("car", 10),
            ("cdr", 11),
            ("null?", 12),
            ("pair?", 13),
            ("list", 14),
            ("display", 15),
            ("newline", 16),
            ("not", 17),
            ("eq?", 18),
            ("equal?", 19),
            ("number?", 20),
            ("symbol?", 21),
            ("string?", 22),
            ("boolean?", 23),
            ("char?", 24),
            ("procedure?", 25),
            ("string-length", 26),
            ("string-ref", 27),
            ("string-append", 28),
            ("number->string", 29),
            ("string->number", 30),
            ("modulo", 31),
            ("remainder", 32),
            ("abs", 33),
            ("min", 34),
            ("max", 35),
            ("zero?", 36),
            ("positive?", 37),
            ("negative?", 38),
            ("even?", 39),
            ("odd?", 40),
            ("length", 41),
            ("append", 42),
            ("reverse", 43),
            ("map", 44),
            ("filter", 45),
            ("fold", 46),    // foldl
            ("for-each", 47),
            ("vector", 48),
            ("vector-ref", 49),
            ("vector-length", 50),
            ("vector->list", 51),
            ("list->vector", 52),
            ("void", 53),
            ("void?", 54),
            ("error", 55),
            ("heap-size", 56),
            ("gc", 57),
            ("hash-of", 58),
            ("cadr", 59),
            ("caddr", 60),
            ("cddr", 61),
            ("caar", 62),
            ("cdar", 63),
            ("list?", 64),
            ("apply", 65),
            ("vector?", 66),
            ("substring", 67),
            ("string->list", 68),
            ("list->string", 69),
            ("char->integer", 70),
            ("integer->char", 71),
            ("expt", 72),
            ("sqrt", 73),
            ("floor", 74),
            ("ceiling", 75),
            ("truncate", 76),
            ("round", 77),
            ("write", 80),
            ("display-string", 81),
            ("load", 82),
            ("gensym", 83),
            ("macroexpand", 84),
        ];

        let env = self.global_env;
        for (name, id) in builtins {
            let sym = self.syms.intern(name);
            self.envs.define(env, sym, Val::builtin(id));
        }

        // Also define 'apply' as a symbol the evaluator recognizes
        // (it's handled specially in eval_apply)
    }

    fn call_builtin(&mut self, id: u32, args: &[Val]) -> Result<Val, String> {
        // Protect builtin arguments from GC (some builtins call eval_in/apply_func)
        let ss_base = self.shadow_stack.len();
        self.shadow_stack.extend_from_slice(args);
        let result = self.call_builtin_inner(id, args);
        self.shadow_stack.truncate(ss_base);
        result
    }

    fn call_builtin_inner(&mut self, id: u32, args: &[Val]) -> Result<Val, String> {
        match id {
            0 => self.builtin_add(args),
            1 => self.builtin_sub(args),
            2 => self.builtin_mul(args),
            3 => self.builtin_div(args),
            4 => self.builtin_eq_num(args),
            5 => self.builtin_lt(args),
            6 => self.builtin_gt(args),
            7 => self.builtin_le(args),
            8 => self.builtin_ge(args),
            9 => self.builtin_cons(args),
            10 => self.builtin_car(args),
            11 => self.builtin_cdr(args),
            12 => Ok(Val::boolean(args.get(0).map_or(false, |v| v.is_nil()))),
            13 => Ok(Val::boolean(
                args.get(0).map_or(false, |v| self.heap.is_cons(*v)),
            )),
            14 => {
                let list = self.heap.list(args);
                Ok(list)
            }
            15 => {
                // display
                for arg in args {
                    self.display_val(*arg);
                }
                Ok(Val::void())
            }
            16 => {
                println!();
                let _ = std::io::stdout().flush();
                Ok(Val::void())
            }
            17 => {
                // not
                let v = args.get(0).ok_or("not: expected argument")?;
                Ok(Val::boolean(!v.is_truthy()))
            }
            18 => {
                // eq?
                let a = args.get(0).ok_or("eq?: expected 2 arguments")?;
                let b = args.get(1).ok_or("eq?: expected 2 arguments")?;
                Ok(Val::boolean(a.0 == b.0))
            }
            19 => {
                // equal? (structural)
                let a = args.get(0).ok_or("equal?: expected 2 arguments")?;
                let b = args.get(1).ok_or("equal?: expected 2 arguments")?;
                Ok(Val::boolean(self.equal(*a, *b)))
            }
            20 => Ok(Val::boolean(
                args.get(0)
                    .map_or(false, |v| v.as_int().is_some()),
            )),
            21 => Ok(Val::boolean(
                args.get(0).map_or(false, |v| v.as_symbol().is_some()),
            )),
            22 => Ok(Val::boolean(
                args.get(0).map_or(false, |v| self.heap.is_string(*v)),
            )),
            23 => Ok(Val::boolean(
                args.get(0).map_or(false, |v| v.as_bool().is_some()),
            )),
            24 => Ok(Val::boolean(
                args.get(0).map_or(false, |v| v.as_char().is_some()),
            )),
            25 => Ok(Val::boolean(args.get(0).map_or(false, |v| {
                v.as_builtin().is_some() || self.heap.is_closure(*v)
            }))),
            26 => {
                // string-length
                let s = self
                    .heap
                    .get_string(*args.get(0).ok_or("string-length: expected string")?)
                    .ok_or("string-length: not a string")?;
                Ok(Val::int(s.len() as i64))
            }
            27 => {
                // string-ref
                let s = self
                    .heap
                    .get_string(*args.get(0).ok_or("string-ref: expected string")?)
                    .ok_or("string-ref: not a string")?;
                let idx = args
                    .get(1)
                    .and_then(|v| v.as_int())
                    .ok_or("string-ref: expected integer index")?;
                let c = s
                    .chars()
                    .nth(idx as usize)
                    .ok_or("string-ref: index out of bounds")?;
                Ok(Val::char_(c))
            }
            28 => {
                // string-append
                let mut result = String::new();
                for arg in args {
                    let s = self
                        .heap
                        .get_string(*arg)
                        .ok_or("string-append: not a string")?;
                    result.push_str(s);
                }
                Ok(self.heap.alloc_string(&result))
            }
            29 => {
                // number->string
                let v = args.get(0).ok_or("number->string: expected number")?;
                let s = if let Some(i) = v.as_int() {
                    i.to_string()
                } else {
                    return Err("number->string: not a number".into());
                };
                Ok(self.heap.alloc_string(&s))
            }
            30 => {
                // string->number
                let s = self
                    .heap
                    .get_string(*args.get(0).ok_or("string->number: expected string")?)
                    .ok_or("string->number: not a string")?;
                if let Ok(i) = s.parse::<i64>() {
                    Ok(Val::int(i))
                } else {
                    Ok(Val::boolean(false))
                }
            }
            31 => {
                // modulo
                let a = args
                    .get(0)
                    .and_then(|v| v.as_int())
                    .ok_or("modulo: expected integer")?;
                let b = args
                    .get(1)
                    .and_then(|v| v.as_int())
                    .ok_or("modulo: expected integer")?;
                if b == 0 {
                    return Err("modulo: division by zero".into());
                }
                Ok(Val::int(((a % b) + b) % b))
            }
            32 => {
                // remainder
                let a = args
                    .get(0)
                    .and_then(|v| v.as_int())
                    .ok_or("remainder: expected integer")?;
                let b = args
                    .get(1)
                    .and_then(|v| v.as_int())
                    .ok_or("remainder: expected integer")?;
                if b == 0 {
                    return Err("remainder: division by zero".into());
                }
                Ok(Val::int(a % b))
            }
            33 => {
                // abs
                let v = args.get(0).ok_or("abs: expected number")?;
                if let Some(i) = v.as_int() {
                    Ok(Val::int(i.abs()))
                } else {
                    Err("abs: not a number".into())
                }
            }
            34 => {
                // min
                self.numeric_fold(args, |a, b| if a < b { a } else { b }, |a, b| if a < b { a } else { b })
            }
            35 => {
                // max
                self.numeric_fold(args, |a, b| if a > b { a } else { b }, |a, b| if a > b { a } else { b })
            }
            36 => {
                // zero?
                let v = args.get(0).ok_or("zero?: expected number")?;
                Ok(Val::boolean(v.as_int() == Some(0)))
            }
            37 => {
                // positive?
                let v = args.get(0).ok_or("positive?: expected number")?;
                if let Some(i) = v.as_int() {
                    Ok(Val::boolean(i > 0))
                } else {
                    Err("positive?: not a number".into())
                }
            }
            38 => {
                // negative?
                let v = args.get(0).ok_or("negative?: expected number")?;
                if let Some(i) = v.as_int() {
                    Ok(Val::boolean(i < 0))
                } else {
                    Err("negative?: not a number".into())
                }
            }
            39 => {
                // even?
                let i = args
                    .get(0)
                    .and_then(|v| v.as_int())
                    .ok_or("even?: expected integer")?;
                Ok(Val::boolean(i % 2 == 0))
            }
            40 => {
                // odd?
                let i = args
                    .get(0)
                    .and_then(|v| v.as_int())
                    .ok_or("odd?: expected integer")?;
                Ok(Val::boolean(i % 2 != 0))
            }
            41 => {
                // length
                let lst = *args.get(0).ok_or("length: expected list")?;
                let v = self.heap.list_to_vec(lst).ok_or("length: not a proper list")?;
                Ok(Val::int(v.len() as i64))
            }
            42 => {
                // append
                if args.is_empty() {
                    return Ok(Val::nil());
                }
                let mut all = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    if i == args.len() - 1 {
                        // Last argument can be any value (improper list tail)
                        if arg.is_nil() {
                            // nothing
                        } else if let Some(v) = self.heap.list_to_vec(*arg) {
                            all.extend(v);
                        } else {
                            // dotted pair at end - we just add all as proper list for simplicity
                            all.push(*arg);
                        }
                    } else {
                        let v = self
                            .heap
                            .list_to_vec(*arg)
                            .ok_or("append: not a proper list")?;
                        all.extend(v);
                    }
                }
                Ok(self.heap.list(&all))
            }
            43 => {
                // reverse
                let lst = *args.get(0).ok_or("reverse: expected list")?;
                let mut v = self.heap.list_to_vec(lst).ok_or("reverse: not a proper list")?;
                v.reverse();
                Ok(self.heap.list(&v))
            }
            44 => {
                // map
                let func = *args.get(0).ok_or("map: expected function")?;
                let lst = *args.get(1).ok_or("map: expected list")?;
                let elems = self.heap.list_to_vec(lst).ok_or("map: not a proper list")?;
                let ss_base = self.shadow_stack.len();
                let mut results = Vec::with_capacity(elems.len());
                for e in &elems {
                    let r = self.apply_func(func, vec![*e])?;
                    let v = match r {
                        Trampoline::Done(v) => v,
                        Trampoline::TailCall { expr, env_id } => {
                            self.eval_in(expr, env_id)?
                        }
                    };
                    self.shadow_stack.push(v);
                    results.push(v);
                }
                let res = self.heap.list(&results);
                self.shadow_stack.truncate(ss_base);
                Ok(res)
            }
            45 => {
                // filter
                let func = *args.get(0).ok_or("filter: expected function")?;
                let lst = *args.get(1).ok_or("filter: expected list")?;
                let elems = self.heap.list_to_vec(lst).ok_or("filter: not a proper list")?;
                let mut results = Vec::new();
                for e in &elems {
                    let r = self.apply_func(func, vec![*e])?;
                    let v = match r {
                        Trampoline::Done(v) => v,
                        Trampoline::TailCall { expr, env_id } => self.eval_in(expr, env_id)?,
                    };
                    if v.is_truthy() {
                        results.push(*e);
                    }
                }
                Ok(self.heap.list(&results))
            }
            46 => {
                // fold (foldl)
                let func = *args.get(0).ok_or("fold: expected function")?;
                let mut acc = *args.get(1).ok_or("fold: expected initial value")?;
                let lst = *args.get(2).ok_or("fold: expected list")?;
                let elems = self.heap.list_to_vec(lst).ok_or("fold: not a proper list")?;
                for e in &elems {
                    let r = self.apply_func(func, vec![*e, acc])?;
                    acc = match r {
                        Trampoline::Done(v) => v,
                        Trampoline::TailCall { expr, env_id } => self.eval_in(expr, env_id)?,
                    };
                }
                Ok(acc)
            }
            47 => {
                // for-each
                let func = *args.get(0).ok_or("for-each: expected function")?;
                let lst = *args.get(1).ok_or("for-each: expected list")?;
                let elems = self.heap.list_to_vec(lst).ok_or("for-each: not a proper list")?;
                for e in &elems {
                    let r = self.apply_func(func, vec![*e])?;
                    match r {
                        Trampoline::Done(_) => {}
                        Trampoline::TailCall { expr, env_id } => {
                            self.eval_in(expr, env_id)?;
                        }
                    }
                }
                Ok(Val::void())
            }
            48 => {
                // vector
                Ok(self.heap.alloc_vector(args.to_vec()))
            }
            49 => {
                // vector-ref
                let vec_val = *args.get(0).ok_or("vector-ref: expected vector")?;
                let idx = args
                    .get(1)
                    .and_then(|v| v.as_int())
                    .ok_or("vector-ref: expected integer index")?;
                let v = self
                    .heap
                    .get_vector(vec_val)
                    .ok_or("vector-ref: not a vector")?;
                v.get(idx as usize)
                    .copied()
                    .ok_or_else(|| "vector-ref: index out of bounds".into())
            }
            50 => {
                // vector-length
                let vec_val = *args.get(0).ok_or("vector-length: expected vector")?;
                let v = self
                    .heap
                    .get_vector(vec_val)
                    .ok_or("vector-length: not a vector")?;
                Ok(Val::int(v.len() as i64))
            }
            51 => {
                // vector->list
                let vec_val = *args.get(0).ok_or("vector->list: expected vector")?;
                let v = self
                    .heap
                    .get_vector(vec_val)
                    .ok_or("vector->list: not a vector")?
                    .to_vec();
                Ok(self.heap.list(&v))
            }
            52 => {
                // list->vector
                let lst = *args.get(0).ok_or("list->vector: expected list")?;
                let v = self
                    .heap
                    .list_to_vec(lst)
                    .ok_or("list->vector: not a proper list")?;
                Ok(self.heap.alloc_vector(v))
            }
            53 => Ok(Val::void()),
            54 => Ok(Val::boolean(
                args.get(0).map_or(false, |v| v.is_void()),
            )),
            55 => {
                // error
                let msg = if let Some(s) = args.get(0) {
                    if let Some(m) = self.heap.get_string(*s) {
                        m.to_string()
                    } else {
                        printer::print_val(*s, &self.heap, &self.syms)
                    }
                } else {
                    "error".to_string()
                };
                Err(msg)
            }
            56 => Ok(Val::int(self.heap.len() as i64)),
            57 => {
                // gc — force garbage collection
                let mut roots = self.envs.all_values();
                roots.extend_from_slice(&self.extra_roots);
                roots.extend_from_slice(&self.shadow_stack);
                roots.extend(self.macros.values().copied());
                let swept = self.heap.gc(&roots);
                Ok(Val::int(swept as i64))
            }
            58 => {
                // hash-of — introspect the hash of a value
                let v = *args.get(0).ok_or("hash-of: expected argument")?;
                if let Some(h) = v.as_heap_ref() {
                    Ok(Val::int(h as i64))
                } else {
                    // For immediates, return the raw bits
                    Ok(Val::int(v.0 as i64))
                }
            }
            59 => {
                // cadr
                let v = *args.get(0).ok_or("cadr: expected pair")?;
                let d = self.heap.cdr(v).ok_or("cadr: not a pair")?;
                self.heap.car(d).ok_or_else(|| "cadr: not a pair".into())
            }
            60 => {
                // caddr
                let v = *args.get(0).ok_or("caddr: expected pair")?;
                let d = self.heap.cdr(v).ok_or("caddr: not a pair")?;
                let dd = self.heap.cdr(d).ok_or("caddr: not a pair")?;
                self.heap.car(dd).ok_or_else(|| "caddr: not a pair".into())
            }
            61 => {
                // cddr
                let v = *args.get(0).ok_or("cddr: expected pair")?;
                let d = self.heap.cdr(v).ok_or("cddr: not a pair")?;
                self.heap.cdr(d).ok_or_else(|| "cddr: not a pair".into())
            }
            62 => {
                // caar
                let v = *args.get(0).ok_or("caar: expected pair")?;
                let a = self.heap.car(v).ok_or("caar: not a pair")?;
                self.heap.car(a).ok_or_else(|| "caar: not a pair".into())
            }
            63 => {
                // cdar
                let v = *args.get(0).ok_or("cdar: expected pair")?;
                let a = self.heap.car(v).ok_or("cdar: not a pair")?;
                self.heap.cdr(a).ok_or_else(|| "cdar: not a pair".into())
            }
            64 => {
                // list?
                let v = *args.get(0).ok_or("list?: expected argument")?;
                Ok(Val::boolean(self.is_list(v)))
            }
            65 => {
                // apply (called as builtin)
                if args.len() != 2 {
                    return Err("apply: expected (apply func args-list)".into());
                }
                let func = args[0];
                let arg_vals = self
                    .heap
                    .list_to_vec(args[1])
                    .ok_or("apply: second argument must be a list")?;
                let r = self.apply_func(func, arg_vals)?;
                match r {
                    Trampoline::Done(v) => Ok(v),
                    Trampoline::TailCall { expr, env_id } => self.eval_in(expr, env_id),
                }
            }
            66 => Ok(Val::boolean(
                args.get(0).map_or(false, |v| self.heap.is_vector(*v)),
            )),
            67 => {
                // substring
                let s = self
                    .heap
                    .get_string(*args.get(0).ok_or("substring: expected string")?)
                    .ok_or("substring: not a string")?;
                let start = args
                    .get(1)
                    .and_then(|v| v.as_int())
                    .ok_or("substring: expected start index")? as usize;
                let end = args
                    .get(2)
                    .and_then(|v| v.as_int())
                    .ok_or("substring: expected end index")? as usize;
                let sub: String = s.chars().skip(start).take(end - start).collect();
                Ok(self.heap.alloc_string(&sub))
            }
            68 => {
                // string->list
                let s = self
                    .heap
                    .get_string(*args.get(0).ok_or("string->list: expected string")?)
                    .ok_or("string->list: not a string")?
                    .to_string();
                let chars: Vec<Val> = s.chars().map(Val::char_).collect();
                Ok(self.heap.list(&chars))
            }
            69 => {
                // list->string
                let lst = *args.get(0).ok_or("list->string: expected list")?;
                let elems = self
                    .heap
                    .list_to_vec(lst)
                    .ok_or("list->string: not a proper list")?;
                let mut s = String::new();
                for e in &elems {
                    let c = e.as_char().ok_or("list->string: element is not a char")?;
                    s.push(c);
                }
                Ok(self.heap.alloc_string(&s))
            }
            70 => {
                // char->integer
                let c = args
                    .get(0)
                    .and_then(|v| v.as_char())
                    .ok_or("char->integer: expected char")?;
                Ok(Val::int(c as i64))
            }
            71 => {
                // integer->char
                let i = args
                    .get(0)
                    .and_then(|v| v.as_int())
                    .ok_or("integer->char: expected integer")?;
                let c = char::from_u32(i as u32).ok_or("integer->char: invalid code point")?;
                Ok(Val::char_(c))
            }
            72 => {
                // expt
                let base = args.get(0).ok_or("expt: expected base")?;
                let exp = args.get(1).ok_or("expt: expected exponent")?;
                match (base.as_int(), exp.as_int()) {
                    (Some(b), Some(e)) if e >= 0 => Ok(Val::int(b.pow(e as u32))),
                    (Some(_), Some(_)) => Err("expt: negative exponent not supported without floats".into()),
                    _ => Err("expt: expected integers".into()),
                }
            }
            73 => {
                // sqrt (integer square root)
                let v = args.get(0).and_then(|v| v.as_int()).ok_or("sqrt: expected integer")?;
                if v < 0 {
                    return Err("sqrt: negative argument".into());
                }
                Ok(Val::int((v as f64).sqrt() as i64))
            }
            74 => {
                // floor — identity on integers
                let v = args.get(0).and_then(|v| v.as_int()).ok_or("floor: expected integer")?;
                Ok(Val::int(v))
            }
            75 => {
                // ceiling — identity on integers
                let v = args.get(0).and_then(|v| v.as_int()).ok_or("ceiling: expected integer")?;
                Ok(Val::int(v))
            }
            76 => {
                // truncate — identity on integers
                let v = args.get(0).and_then(|v| v.as_int()).ok_or("truncate: expected integer")?;
                Ok(Val::int(v))
            }
            77 => {
                // round — identity on integers
                let v = args.get(0).and_then(|v| v.as_int()).ok_or("round: expected integer")?;
                Ok(Val::int(v))
            }
            80 => {
                // write (with quoting for strings/chars)
                for arg in args {
                    let s = printer::print_val(*arg, &self.heap, &self.syms);
                    print!("{s}");
                }
                let _ = std::io::stdout().flush();
                Ok(Val::void())
            }
            81 => {
                // display-string (raw string)
                let s = self
                    .heap
                    .get_string(*args.get(0).ok_or("display-string: expected string")?)
                    .ok_or("display-string: not a string")?;
                print!("{s}");
                let _ = std::io::stdout().flush();
                Ok(Val::void())
            }
            82 => {
                // load — read and evaluate a file
                let path = self
                    .heap
                    .get_string(*args.get(0).ok_or("load: expected a filename string")?)
                    .ok_or("load: argument must be a string")?;
                let contents = std::fs::read_to_string(&path)
                    .map_err(|e| format!("load: {e}"))?;
                let exprs = parser::parse(&contents, &mut self.heap, &mut self.syms)
                    .map_err(|e| format!("load: parse error: {e}"))?;
                let prev_roots = std::mem::replace(&mut self.extra_roots, exprs.clone());
                let mut last = Val::void();
                for (i, expr) in exprs.iter().enumerate() {
                    last = self.eval(*expr)?;
                    self.extra_roots[i] = Val::nil();
                }
                self.extra_roots = prev_roots;
                Ok(last)
            }
            83 => {
                // gensym — generate a unique symbol
                self.gensym_counter += 1;
                let name = format!("__g{}", self.gensym_counter);
                let sym_id = self.syms.intern(&name);
                Ok(Val::symbol(sym_id))
            }
            84 => {
                // macroexpand — expand a macro form once (for debugging)
                let form = *args.get(0).ok_or("macroexpand: expected argument")?;
                if form.is_heap() && self.heap.is_cons(form) {
                    let car = self.heap.car(form).unwrap();
                    if let Some(sym_id) = car.as_symbol() {
                        if let Some(&transformer) = self.macros.get(&sym_id) {
                            let cdr = self.heap.cdr(form).unwrap();
                            let arg_vals = self.heap.list_to_vec(cdr)
                                .ok_or("macroexpand: args must be a proper list")?;
                            let expanded = self.apply_func(transformer, arg_vals)?;
                            return match expanded {
                                Trampoline::Done(v) => Ok(v),
                                Trampoline::TailCall { expr, env_id } => self.eval_in(expr, env_id),
                            };
                        }
                    }
                }
                // Not a macro form, return as-is
                Ok(form)
            }
            _ => Err(format!("unknown builtin id: {id}")),
        }
    }

    // ── Arithmetic helpers ──

    fn to_int(&self, v: Val) -> Result<i64, String> {
        if let Some(i) = v.as_int() {
            Ok(i)
        } else {
            Err("expected a number".into())
        }
    }

    fn builtin_add(&self, args: &[Val]) -> Result<Val, String> {
        let mut sum: i64 = 0;
        for v in args {
            if let Some(i) = v.as_int() {
                sum += i;
            } else {
                return Err("+: expected numbers".into());
            }
        }
        Ok(Val::int(sum))
    }

    fn builtin_sub(&self, args: &[Val]) -> Result<Val, String> {
        if args.is_empty() {
            return Err("-: expected at least one argument".into());
        }
        if args.len() == 1 {
            if let Some(i) = args[0].as_int() {
                return Ok(Val::int(-i));
            }
            return Err("-: expected number".into());
        }
        let mut result = self.to_int(args[0])?;
        for v in &args[1..] {
            result -= self.to_int(*v)?;
        }
        Ok(Val::int(result))
    }

    fn builtin_mul(&self, args: &[Val]) -> Result<Val, String> {
        let mut prod: i64 = 1;
        for v in args {
            if let Some(i) = v.as_int() {
                prod *= i;
            } else {
                return Err("*: expected numbers".into());
            }
        }
        Ok(Val::int(prod))
    }

    fn builtin_div(&self, args: &[Val]) -> Result<Val, String> {
        if args.is_empty() {
            return Err("/: expected at least one argument".into());
        }
        let mut result = self.to_int(args[0])?;
        if args.len() == 1 {
            if result == 0 {
                return Err("/: division by zero".into());
            }
            return Ok(Val::int(1 / result));
        }
        for v in &args[1..] {
            let n = self.to_int(*v)?;
            if n == 0 {
                return Err("/: division by zero".into());
            }
            result /= n;
        }
        Ok(Val::int(result))
    }

    fn builtin_eq_num(&self, args: &[Val]) -> Result<Val, String> {
        if args.len() < 2 {
            return Ok(Val::boolean(true));
        }
        let first = self.to_int(args[0])?;
        for v in &args[1..] {
            if self.to_int(*v)? != first {
                return Ok(Val::boolean(false));
            }
        }
        Ok(Val::boolean(true))
    }

    fn builtin_lt(&self, args: &[Val]) -> Result<Val, String> {
        self.numeric_cmp(args, |a, b| a < b)
    }

    fn builtin_gt(&self, args: &[Val]) -> Result<Val, String> {
        self.numeric_cmp(args, |a, b| a > b)
    }

    fn builtin_le(&self, args: &[Val]) -> Result<Val, String> {
        self.numeric_cmp(args, |a, b| a <= b)
    }

    fn builtin_ge(&self, args: &[Val]) -> Result<Val, String> {
        self.numeric_cmp(args, |a, b| a >= b)
    }

    fn numeric_cmp(&self, args: &[Val], cmp: fn(i64, i64) -> bool) -> Result<Val, String> {
        if args.len() < 2 {
            return Ok(Val::boolean(true));
        }
        let mut prev = self.to_int(args[0])?;
        for v in &args[1..] {
            let curr = self.to_int(*v)?;
            if !cmp(prev, curr) {
                return Ok(Val::boolean(false));
            }
            prev = curr;
        }
        Ok(Val::boolean(true))
    }

    fn numeric_fold(
        &self,
        args: &[Val],
        int_op: fn(i64, i64) -> i64,
        _float_op: fn(f64, f64) -> f64,
    ) -> Result<Val, String> {
        if args.is_empty() {
            return Err("expected at least one argument".into());
        }
        let mut acc = self.to_int(args[0])?;
        for v in &args[1..] {
            acc = int_op(acc, self.to_int(*v)?);
        }
        Ok(Val::int(acc))
    }

    fn builtin_cons(&mut self, args: &[Val]) -> Result<Val, String> {
        let car = *args.get(0).ok_or("cons: expected 2 arguments")?;
        let cdr = *args.get(1).ok_or("cons: expected 2 arguments")?;
        Ok(self.heap.cons(car, cdr))
    }

    fn builtin_car(&self, args: &[Val]) -> Result<Val, String> {
        let v = *args.get(0).ok_or("car: expected pair")?;
        self.heap.car(v).ok_or_else(|| "car: not a pair".into())
    }

    fn builtin_cdr(&self, args: &[Val]) -> Result<Val, String> {
        let v = *args.get(0).ok_or("cdr: expected pair")?;
        self.heap.cdr(v).ok_or_else(|| "cdr: not a pair".into())
    }

    fn display_val(&self, val: Val) {
        if let Some(s) = self.heap.get_string(val) {
            print!("{s}");
        } else {
            print!("{}", printer::print_val(val, &self.heap, &self.syms));
        }
        let _ = std::io::stdout().flush();
    }

    fn equal(&self, a: Val, b: Val) -> bool {
        if a.0 == b.0 {
            return true;
        }
        // Structural comparison for heap objects
        if let (Some(ah), Some(bh)) = (a.as_heap_ref(), b.as_heap_ref()) {
            match (self.heap.get(ah), self.heap.get(bh)) {
                (Some(crate::heap::HeapObject::Cons(a1, a2)), Some(crate::heap::HeapObject::Cons(b1, b2))) => {
                    self.equal(*a1, *b1) && self.equal(*a2, *b2)
                }
                (Some(crate::heap::HeapObject::Str(s1)), Some(crate::heap::HeapObject::Str(s2))) => s1 == s2,
                (Some(crate::heap::HeapObject::Vector(v1)), Some(crate::heap::HeapObject::Vector(v2))) => {
                    v1.len() == v2.len() && v1.iter().zip(v2.iter()).all(|(a, b)| self.equal(*a, *b))
                }
                _ => false,
            }
        } else {
            false
        }
    }

    fn is_list(&self, val: Val) -> bool {
        let mut current = val;
        loop {
            if current.is_nil() {
                return true;
            }
            if !self.heap.is_cons(current) {
                return false;
            }
            current = self.heap.cdr(current).unwrap();
        }
    }
}
