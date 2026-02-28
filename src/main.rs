mod env;
mod eval;
mod heap;
mod parser;
mod printer;
mod symbol;
mod value;

use std::env as std_env;
use std::fs;

use eval::Evaluator;
use printer::print_val;
use value::Val;

fn run_repl(eval: &mut Evaluator) {
    let mut rl = rustyline::DefaultEditor::new().expect("failed to create readline editor");
    println!("Hashlisp v0.1.0 — a hash-consed Scheme");
    println!("  Heap is a hash table. Identical structures share the same hash.");
    println!("  Type (heap-size) to inspect, (gc) to collect, (hash-of x) to see hashes.");
    println!("  Ctrl-D to exit.\n");

    loop {
        let readline = rl.readline("hashlisp> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                let _ = rl.add_history_entry(line);
                run_input(eval, line, true);
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                println!("\nBye!");
                break;
            }
            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(err) => {
                eprintln!("Error: {err}");
                break;
            }
        }
    }
}

fn run_input(eval: &mut Evaluator, input: &str, print_result: bool) {
    match parser::parse(input, &mut eval.heap, &mut eval.syms) {
        Err(e) => eprintln!("Parse error: {e}"),
        Ok(exprs) => {
            // Register all parsed expressions as GC roots so string literals
            // and other heap objects in the AST don't get collected prematurely.
            eval.extra_roots = exprs.clone();
            for (i, expr) in exprs.iter().enumerate() {
                match eval.eval(*expr) {
                    Ok(val) => {
                        if print_result && !val.is_void() {
                            println!("{}", print_val(val, &eval.heap, &eval.syms));
                        }
                    }
                    Err(e) => {
                        eprintln!("Error: {e}");
                    }
                }
                // Remove evaluated expression from roots
                eval.extra_roots[i] = Val::nil();
            }
            eval.extra_roots.clear();
        }
    }
}

fn main() {
    let args: Vec<String> = std_env::args().collect();
    let mut eval = Evaluator::new();

    if args.len() > 1 {
        // Run file(s)
        for path in &args[1..] {
            match fs::read_to_string(path) {
                Ok(src) => run_input(&mut eval, &src, false),
                Err(e) => eprintln!("Error reading {path}: {e}"),
            }
        }
    } else {
        // REPL
        run_repl(&mut eval);
    }
}
