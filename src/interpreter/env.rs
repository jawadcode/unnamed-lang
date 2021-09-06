use std::collections::HashMap;

use crate::{interpreter::InterpreterError, lexer::token::Span};

use super::{value::Value, ValueResult};

pub type Scope = HashMap<String, Value>;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    scopes: Vec<Scope>,
    depth: usize,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            depth: 0,
        }
    }
}

impl Env {
    pub fn get(&self, name: &str, span: Span) -> ValueResult {
        for depth in (0..=self.depth).rev() {
            if let Some(value) = self.scopes[depth].get(name).map(Clone::clone) {
                return Ok(value);
            }
        }

        Err(InterpreterError::Undefined {
            span,
            ident: name.to_string(),
        })
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.scopes.get_mut(self.depth).unwrap().insert(name, value);
    }

    pub fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
        self.depth += 1;
    }

    pub fn get_innermost_scope(&self) -> Scope {
        self.scopes.last().unwrap().clone()
    }

    pub fn new_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.depth += 1;
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
        self.depth -= 1;
    }
}
