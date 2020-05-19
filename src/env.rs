use std::cell::RefCell;
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
    symbols: RefCell<Vec<HashMap<String, Symbol>>>,
    pub extensions_gnu: bool,
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
            symbols: vec![HashMap::default()].into(),
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
        Env {
            extensions_gnu: true,
            symbols: vec![symbols].into(),
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
            symbols: vec![symbols].into(),
            reserved: reserved,
            ..Default::default()
        }
    }

    pub fn enter_scope(&self) {
        self.symbols.borrow_mut().push(HashMap::new());
    }

    pub fn leave_scope(&self) {
        self.symbols
            .borrow_mut()
            .pop()
            .expect("more scope pops than pushes");
    }

    pub fn is_typename(&self, ident: &str) -> bool {
        for scope in self.symbols.borrow().iter().rev() {
            if let Some(symbol) = scope.get(ident) {
                return *symbol == Symbol::Typename;
            }
        }
        false
    }

    pub fn handle_declarator(&self, d: &Node<Declarator>, sym: Symbol) {
        if let Some(name) = find_declarator_name(&d.node.kind.node) {
            self.add_symbol(name, sym)
        }
    }

    pub fn add_symbol(&self, s: &str, symbol: Symbol) {
        let mut symbols = self.symbols.borrow_mut();
        let scope = symbols
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
