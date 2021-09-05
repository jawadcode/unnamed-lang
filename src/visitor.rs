use crate::{
    ast::{Expr, Stmt},
    parser::Spanned,
};

pub trait Visitor<T, U> {
    fn visit_stmt(&mut self, s: &Spanned<Stmt>) -> U;
    fn visit_expr(&mut self, e: &Spanned<Expr>) -> T;
}
