///! Symbol table — a global interning table so that identical symbol names
///! map to the same u32 id. Symbols are immediate values (no heap allocation).

use std::collections::HashMap;

pub struct SymbolTable {
    /// name → id
    map: HashMap<String, u32>,
    /// id → name
    names: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            map: HashMap::new(),
            names: Vec::new(),
        }
    }

    /// Intern a symbol name, returning its id.
    pub fn intern(&mut self, name: &str) -> u32 {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        let id = self.names.len() as u32;
        self.names.push(name.to_string());
        self.map.insert(name.to_string(), id);
        id
    }

    /// Look up name by id.
    pub fn name(&self, id: u32) -> &str {
        &self.names[id as usize]
    }
}
