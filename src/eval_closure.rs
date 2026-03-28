use std::collections::HashMap;
use std::io::Write;
use std::cell::RefCell;

use crate::heap::Heap;
use crate::{eval, parser};
use crate::symbol::SymbolTable;
use crate::value::{Val, TAG_INT, TAG_BOOL, TAG_NIL, TAG_CHAR, TAG_SYMBOL, TAG_HEAP, TAG_BUILTIN, TAG_VOID};
use crate::printer;

// In this evaluator, the environment doesn't contain symbols. All symbols are
// resolved during traversal and environments are accessed using the index
// instead. So the environment is just a hash-consed list of values.

pub(crate) fn env_get(heap: &Heap, env: Val, ix: u32) -> Option<Val> {
  let mut current = env;
  let mut ix = ix;
  while !current.is_nil() && ix > 0 {
    current = heap.cdr(current)?;
    ix -= 1;
  }
  if current.is_nil() {
    None
  } else {
    heap.car(current)
  }
}

pub(crate) struct EvalClosure<'s>(Box<dyn 's + Fn(&ExecutionContext) -> Option<Val>>);

impl<'s> EvalClosure<'s> {
  pub(crate) fn new(closure : impl 's + Fn(&ExecutionContext) -> Option<Val>) -> Self {
    EvalClosure(Box::new(closure))
  }

  pub(crate) fn call(&self, ctx: &ExecutionContext) -> Option<Val> {
    (self.0)(ctx)
  }
}

pub struct ExecutionContext<'h> {
  pub heap: &'h Heap,
  pub env: Val,
}

impl <'h> ExecutionContext<'h> {
  pub fn new(heap: &'h Heap, env: Val) -> Self {
    ExecutionContext { heap, env }
  }
}

pub struct Traverse<'h> {

  // The AST lives in the heap, so we need to be able to read it during traversal.
  heap: &'h Heap,
  pub symbol_table: &'h SymbolTable,
  pub env: RefCell<HashMap<u32, u32>>,
  pub env_global: RefCell<HashMap<u32, Val>>,

}

impl <'h> Traverse<'h> {
  const sym_quote : u32 = 0;
  const sym_if : u32 = 1;
  const sym_cond : u32 = 2;
  const sym_define : u32 = 3;
  const sym_lambda : u32 = 4;
  const sym_let : u32 = 5;
  const sym_letstar : u32 = 6;
  const sym_letrec : u32 = 7;
  const sym_begin : u32 = 8;
  const sym_and : u32 = 9;
  const sym_or : u32 = 10;
  const sym_else : u32 = 11;
  const sym_do : u32 = 12;
  const sym_when : u32 = 13;
  const sym_unless : u32 = 14;
  const sym_apply : u32 = 15;
  const sym_quasiquote : u32 = 16;
  const sym_unquote : u32 = 17;
  const sym_unquote_splicing : u32 = 18;
  const sym_define_macro : u32 = 19;
  const sym_define_memo : u32 = 20;

  pub fn new(heap: &'h Heap, symbol_table: &'h SymbolTable) -> Self {
    Traverse {
      heap,
      symbol_table,
      env: HashMap::new().into(),
      env_global: HashMap::new().into(),
    }
  }

  pub fn eval(&self, expr : Val) -> Result<EvalClosure,String> {
    match expr.tag() {
      TAG_INT | TAG_BOOL | TAG_NIL | TAG_CHAR | TAG_BUILTIN | TAG_VOID => {
        Ok(EvalClosure::new(move |_| {
          println!("Evaluating literal: {}", printer::print_val(expr, self.heap, &self.symbol_table));
          Some(expr)
        }
        ))
      }

      TAG_SYMBOL => {
        println!("Evaluating symbol: {}", self.symbol_table.name(expr.payload() as u32));
        let sym_id = expr.payload() as u32;
        if let Some(ix) = self.env.borrow().get(&sym_id) {
          let ix = *ix;
          return Ok(EvalClosure::new(
            move |ctx| env_get(&ctx.heap, ctx.env, ix)));
        }
        // I'm not sure this is correct. We might have to check the global
        // environment at runtime, rather than during traversal.
        if let Some (val) = self.env_global.borrow().get(&sym_id) {
          let val = *val;
          Ok(EvalClosure::new(move |_ctx| Some(val)))
        } else {
          Err(format!("unbound variable: {}", sym_id)) // TODO: print symbol name
        }
      }
      TAG_HEAP => {
        if self.heap.is_cons(expr) {
          return self.eval_cons(expr);
        }
        // Heap references are already evaluated.
        Ok(EvalClosure::new(move |_| Some(expr)))
      }
      _ => Err(format!("cannot evaluate: {:?}", expr)),

    }
  }

  fn eval_cons(&self, expr: Val) -> Result<EvalClosure, String> {
    let car = self.heap.car(expr).unwrap();
    let cdr = self.heap.cdr(expr).unwrap();
    match car.as_symbol() {
      Some(sym_id) => {
        match sym_id {
          0 => { // quote
            let quoted = self.heap.car(cdr).ok_or("malformed quote: missing argument")?;
            Ok(EvalClosure::new(move |_| Some(quoted)))
          }
          1 => { // if
            let cond = self.heap.car(cdr).unwrap();
            let then_branch = self.heap.car(self.heap.cdr(cdr).unwrap()).unwrap();
            let else_branch = self.heap.car(self.heap.cdr(self.heap.cdr(cdr).unwrap()).unwrap()).unwrap();

            let old_env = self.env.borrow().clone();
            let eval_cond: EvalClosure<'_> = self.eval(cond)?;
            self.env.replace(old_env.clone());
            let eval_then = self.eval(then_branch)?;
            self.env.replace(old_env.clone());
            let eval_else = self.eval(else_branch)?;
            self.env.replace(old_env);
            Ok(EvalClosure::new(move |ctx| {
              let bool = eval_cond.call(ctx)?;
              println!("Condition evaluated to: {}", printer::print_val(bool, ctx.heap, &self.symbol_table));
              if !bool.is_nil() {
                eval_then.call(ctx) // TODO: tail call
              } else {
                eval_else.call(ctx) // TODO: tail call
              }
            }))
          }
          2 => { // cond
            let clauses = self.heap.list_to_vec(expr).ok_or("cond: expected list of clauses")?;
            let closures = clauses.iter().map(|clause| {
              let parts = self.heap.list_to_vec(*clause).ok_or("cond: expected clause to be a list")?;
              if parts.is_empty() {
                return Err("cond: clause cannot be empty".to_string());
              }
              let test = parts[0];
              let eval_test = self.eval(test)?;
              if parts.len() == 1 {
                Ok((eval_test, None))
              } else {
                let eval_parts : Vec<EvalClosure>= parts.iter().map(|part|{
                  self.eval(*part)
                }).collect::<Result<Vec<_>, _>>()?;
                Ok((eval_test,Some(
                  EvalClosure::new(move |ctx| {
                    let mut result = Val::nil();
                    for eval_part in &eval_parts {
                      result = eval_part.call(ctx)?; // TODO: Fail properly
                      // TODO: Tail call
                    }
                    Some(result)
                  }))))
                }
              }
            ).collect::<Result<Vec<_>, _>>()?;
            Ok(EvalClosure::new(move |ctx| {
              for (eval_test,eval_clause_o) in &closures {
                match eval_clause_o {
                  None => {
                    let bool = eval_test.call(ctx)?;
                    if !bool.is_nil() {
                      return Some(bool);
                    }
                    continue;
                  }
                  Some(eval_clause) => {
                    let bool = eval_test.call(ctx)?;
                    if !bool.is_nil() {
                      return eval_clause.call(ctx); // TODO: Tail call
                    }
                  }
                }
              }
              None
          }))
          }
          3 => { // define
            let var = self.heap.car(cdr).ok_or("define: missing variable")?;
            let val = self.heap.car(self.heap.cdr(cdr).unwrap()).ok_or("define: missing value")?;
            let var_sym_id = var.as_symbol().ok_or("define: expected symbol as variable")?;
            self.env_global.borrow_mut().insert(var_sym_id, val);
            Ok(EvalClosure::new(move |_ctx| {
              Some(Val::nil())
            }))
          }
          4 => { // lambda
            self.eval_lambda(cdr)
          }
          _ => Err(format!("unsupported special form: {}", self.symbol_table.name(sym_id))),
        }
      },
      _ => Err("first element of list is not a symbol".to_string()),
    }
  }

  fn eval_lambda(&self, args: Val) -> Result<EvalClosure, String> {
    let params_form = self.heap.car(args).ok_or("lambda: missing parameters")?;
    let body_form = self.heap.car(self.heap.cdr(args).unwrap()).ok_or("lambda: missing body")?;

    let param_syms = self.heap.list_to_vec(params_form).ok_or("lambda: expected parameter list")?;

    Err("lambda not implemented yet".to_string())
  }
}

pub(crate) struct ClosureEvaluator {
  pub heap: Heap,
  pub syms: SymbolTable,
}

impl ClosureEvaluator {
  pub fn new() -> Self {
    let mut syms = SymbolTable::new();
    syms.intern("quote");
    syms.intern("if");
    syms.intern("cond");
    syms.intern("define");
    syms.intern("lambda");
    syms.intern("let");
    syms.intern("let*");
    syms.intern("letrec");
    syms.intern("begin");
    syms.intern("and");
    syms.intern("or");
    syms.intern("else");
    syms.intern("do");
    syms.intern("when");
    syms.intern("unless");
    syms.intern("apply");
    syms.intern("quasiquote");
    syms.intern("unquote");
    syms.intern("unquote-splicing");
    syms.intern("define-macro");
    syms.intern("define-memo");
    ClosureEvaluator {
      heap: Heap::new(),
      syms: syms,
    }
  }

  pub fn eval(&self, expr: Val) -> Result<Val, String> {
    let traverse = Traverse::new(&self.heap, &self.syms);
    let closure = traverse.eval(expr)?;
    closure.call(&ExecutionContext::new(&self.heap, Val::nil())).ok_or("evaluation error".to_string())
  }
}