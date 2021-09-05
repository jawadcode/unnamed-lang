pub mod impls;

use crate::{lexer::token_kind::TokenKind, parser::Spanned};

type Boxpr = Box<Spanned<Expr>>;
type Exprs = Vec<Spanned<Expr>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Ident(String),
    If {
        cond: Boxpr,
        true_value: Boxpr,
        false_value: Boxpr,
    },
    Match {
        expr: Boxpr,
        arms: Vec<MatchArm>,
    },
    Block {
        exprs: Exprs,
    },
    BinaryOp {
        op: TokenKind,
        lhs: Boxpr,
        rhs: Boxpr,
    },
    UnaryOp {
        op: TokenKind,
        expr: Boxpr,
    },
    FnCall {
        fun: Boxpr,
        args: Exprs,
    },
    Closure(Function),
    Stmt(Spanned<Stmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    FnDef { ident: String, fun: Function },
    Let { ident: String, expr: Boxpr },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Unit,
    Bool(bool),
    Int(usize),
    Float(f64),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub ident: Option<String>,
    pub params: Vec<String>,
    pub body: Boxpr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pattern: Exprs,
    pub result: Boxpr,
}
