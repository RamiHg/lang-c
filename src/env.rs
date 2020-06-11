use std::collections::{HashMap, HashSet};

use ast::*;
use span::Node;
use strings;

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum Symbol {
    Typename,
    Identifier,
}

#[derive(Default)]
pub struct Env {
    symbols: Vec<HashMap<String, Symbol>>,
    pub extensions_gnu: bool,
    pub extensions_strict_gnu: bool,
    pub extensions_clang: bool,
    pub extensions_windows: bool,
    pub reserved: HashSet<&'static str>,
}

impl Env {
    #[cfg(test)]
    pub fn new() -> Env {
        Env::with_gnu()
    }

    pub fn with_core() -> Env {
        let mut reserved = HashSet::default();
        reserved.extend(strings::RESERVED_C11.iter());
        Env {
            symbols: vec![HashMap::default()],
            reserved: reserved,
            ..Default::default()
        }
    }

    pub fn with_gnu() -> Env {
        let mut symbols = HashMap::default();
        let mut reserved = HashSet::default();
        symbols.insert("__builtin_va_list".to_owned(), Symbol::Typename);
        reserved.extend(strings::RESERVED_C11.iter());
        reserved.extend(strings::RESERVED_GNU.iter());
        reserved.extend(strings::RESERVED_STRICT_GNU.iter());
        Env {
            extensions_gnu: true,
            extensions_strict_gnu: true,
            symbols: vec![symbols],
            reserved: reserved,
            ..Default::default()
        }
    }

    pub fn with_clang() -> Env {
        let mut symbols = HashMap::default();
        let mut reserved = HashSet::default();
        symbols.insert("__builtin_va_list".to_owned(), Symbol::Typename);
        reserved.extend(strings::RESERVED_C11.iter());
        reserved.extend(strings::RESERVED_GNU.iter());
        reserved.extend(strings::RESERVED_CLANG.iter());
        #[cfg(target_os = "windows")]
        reserved.extend(strings::RESERVED_WINDOWS.iter());
        Env {
            extensions_gnu: true,
            extensions_clang: true,
            extensions_windows: true,
            symbols: vec![symbols],
            reserved: reserved,
            ..Default::default()
        }
    }

    pub fn enter_scope(&mut self) {
        self.symbols.push(HashMap::new());
    }

    pub fn leave_scope(&mut self) {
        self.symbols.pop().expect("more scope pops than pushes");
    }

    pub fn is_typename(&self, ident: &str) -> bool {
        for scope in self.symbols.iter().rev() {
            if let Some(symbol) = scope.get(ident) {
                return *symbol == Symbol::Typename;
            }
        }
        false
    }

    pub fn handle_declarator(&mut self, d: &Node<Declarator>, sym: Symbol) {
        if let Some(name) = find_declarator_name(&d.node.kind.node) {
            self.add_symbol(name, sym)
        }
    }

    pub fn add_symbol(&mut self, s: &str, symbol: Symbol) {
        let scope = self
            .symbols
            .last_mut()
            .expect("at least one scope should be always present");
        scope.insert(s.to_string(), symbol);
    }

    #[cfg(test)]
    pub fn add_typename(&mut self, s: &str) {
        self.add_symbol(s, Symbol::Typename)
    }
}

fn find_declarator_name(d: &DeclaratorKind) -> Option<&str> {
    match d {
        &DeclaratorKind::Abstract => None,
        &DeclaratorKind::Identifier(ref i) => Some(&i.node.name),
        &DeclaratorKind::Declarator(ref d) => find_declarator_name(&d.node.kind.node),
    }
}
