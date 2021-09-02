use std::fmt;

use crate::ast::Expr;

use super::{Function, Lit, MatchArm, Stmt};

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(s) => s.fmt(f),
            Self::Ident(s) => s.fmt(f),
            Self::If {
                cond,
                true_value,
                false_value,
            } => write!(
                f,
                "(if :cond {} :then {} :else {})",
                cond, true_value, false_value
            ),
            Self::Match { expr, arms } => {
                write!(f, "(match :target {} :arms ({}))", expr, join(arms))
            }
            Self::Block { exprs } => write!(f, "(block ({}))", join(exprs)),
            Self::BinaryOp { op, lhs, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Self::UnaryOp { op, expr } => write!(f, "({} {})", op, expr),
            Self::FnCall { fun, args } => write!(f, "({} {})", fun, join(args)),
            Self::Closure(fun) => write!(f, "(lambda {})", fun),
            Self::Stmt(s) => s.fmt(f),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FnDef { ident, fun } => write!(f, "(define {} {})", ident, fun),
            Self::Let { ident, expr } => write!(f, "(let {} {})", ident, expr),
        }
    }
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

impl fmt::Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(({}) {})", join(&self.pattern), self.result)
    }
}

#[inline(always)]
fn join(vec: &[impl ToString]) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" ")
}
