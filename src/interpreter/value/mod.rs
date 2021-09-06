pub mod impls;

use std::fmt::{self, Debug};

use crate::ast::Function;

use super::env::{Env, Scope};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Function),
    BuiltinFn(BuiltinFn),
    Closure(Closure),
}

#[derive(Clone)]
pub struct BuiltinFn {
    pub ident: String,
    pub params: Vec<String>,
    pub body: fn(&mut Env, &[Value]) -> Value,
}

impl fmt::Debug for BuiltinFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin {}({})>", self.ident, self.params.join(", "))
    }
}

impl PartialEq for BuiltinFn {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.params == other.params
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub scope: Scope,
    pub fun: Function,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => fmt::Display::fmt(&"()", f),
            Self::Bool(b) => fmt::Display::fmt(b, f),
            Self::Number(n) => fmt::Display::fmt(n, f),
            Self::String(s) => fmt::Display::fmt(s, f),
            Self::Function(fun) => match &fun.ident {
                Some(s) => write!(f, "<function {}({})>", s, &fun.params.join(", ")),
                None => write!(f, "<fn({})>", &fun.params.join(", ")),
            },
            Self::BuiltinFn(fun) => fun.fmt(f),
            Self::Closure(c) => write!(f, "<closure({})>", &c.fun.params.join(", ")),
        }
    }
}
