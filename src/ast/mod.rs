use std::fmt;

use crate::{lexer::token_kind::TokenKind, parser::Spanned};

type Boxpr = Box<Spanned<Expr>>;
type Exprs = Vec<Spanned<Expr>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Boxpr,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            ":params ({}) :body {}",
            self.params.join(" "),
            self.body.node
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    FnDef { ident: String, fun: Function },
    Let { ident: String, expr: Boxpr },
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FnDef { ident, fun } => write!(f, "(define {} {})", ident, fun),
            Self::Let { ident, expr } => write!(f, "(let {} {})", ident, expr),
        }
    }
}

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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(s) => s.fmt(f),
            Expr::Ident(s) => s.fmt(f),
            Expr::If {
                cond,
                true_value,
                false_value,
            } => write!(
                f,
                "(if :cond {} :then {} :else {})",
                cond, true_value, false_value
            ),
            Expr::Match { expr, arms } => {
                write!(f, "(match :target {} :arms ({}))", expr, join(arms))
            }
            Expr::Block { exprs } => write!(f, "(block ({}))", join(exprs)),
            Expr::BinaryOp { op, lhs, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Expr::UnaryOp { op, expr } => write!(f, "({} {})", op, expr),
            Expr::FnCall { fun, args } => write!(f, "({} {})", fun, join(args)),
            Expr::Closure(fun) => write!(f, "(lambda {})", fun),
            Expr::Stmt(s) => s.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pattern: Exprs,
    pub result: Boxpr,
}

impl fmt::Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(({}) {})", join(&self.pattern), self.result)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Unit,
    Bool(bool),
    Int(usize),
    Float(f64),
    String(String),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(fl) => write!(f, "{}", fl),
            Self::String(s) => write!(f, r#""{}""#, s),
        }
    }
}

#[inline(always)]
fn join(vec: &[impl ToString]) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" ")
}
