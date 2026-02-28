///! Environments for Hashlisp.
///!
///! An environment is a mapping from symbol ids to Vals, with a parent chain.
///! Environments are stored in a Vec (arena) and referenced by index.

use std::collections::HashMap;

use crate::value::Val;

struct Env {
    bindings: HashMap<u32, Val>,
    parent: Option<usize>,
}

pub struct EnvStore {
    envs: Vec<Env>,
}

impl EnvStore {
    pub fn new() -> Self {
        EnvStore { envs: Vec::new() }
    }

    /// Create a new empty top-level environment.
    pub fn new_top_level(&mut self) -> usize {
        let id = self.envs.len();
        self.envs.push(Env {
            bindings: HashMap::new(),
            parent: None,
        });
        id
    }

    /// Create a child environment.
    pub fn new_child(&mut self, parent: usize) -> usize {
        let id = self.envs.len();
        self.envs.push(Env {
            bindings: HashMap::new(),
            parent: Some(parent),
        });
        id
    }

    /// Define a binding in the given environment.
    pub fn define(&mut self, env_id: usize, sym: u32, val: Val) {
        self.envs[env_id].bindings.insert(sym, val);
    }

    /// Set an existing binding (walks up parent chain).
    pub fn set(&mut self, env_id: usize, sym: u32, val: Val) -> bool {
        if self.envs[env_id].bindings.contains_key(&sym) {
            self.envs[env_id].bindings.insert(sym, val);
            return true;
        }
        if let Some(parent) = self.envs[env_id].parent {
            self.set(parent, sym, val)
        } else {
            false
        }
    }

    /// Look up a binding (walks up parent chain).
    pub fn get(&self, env_id: usize, sym: u32) -> Option<Val> {
        if let Some(&val) = self.envs[env_id].bindings.get(&sym) {
            return Some(val);
        }
        if let Some(parent) = self.envs[env_id].parent {
            self.get(parent, sym)
        } else {
            None
        }
    }

    /// Collect all values reachable from an environment (for GC roots).
    pub fn all_values(&self) -> Vec<Val> {
        let mut vals = Vec::new();
        for env in &self.envs {
            for &v in env.bindings.values() {
                vals.push(v);
            }
        }
        vals
    }
}
