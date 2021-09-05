pub mod env;
pub mod error;
pub mod expr;
pub mod stmt;
pub mod value;

use std::fmt;

use crate::{
    ast::{Expr, Stmt},
    lexer::token::Span,
    parser::Spanned,
    visitor::Visitor,
};

use self::{
    env::Env,
    error::InterpreterError,
    value::{BuiltinFn, Value},
};

pub type InterpreterResult<T> = Result<T, InterpreterError>;
pub type ValueResult = InterpreterResult<Value>;

pub type SpanExpr = Spanned<Expr>;
pub type SpanStmt = Spanned<Stmt>;

#[derive(Debug)]
pub struct Interpreter {
    env: Env,
}

impl Interpreter {
    pub fn run(&mut self, stmts: &[SpanStmt]) -> InterpreterResult<()> {
        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }

        let span: Span = (0..0).into();
        self.visit_fncall(
            span,
            &Spanned {
                span,
                node: Expr::Ident("main".to_string()),
            },
            &[],
        )
        .map_err(|_| InterpreterError::MissingMain)?;
        Ok(())
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut env = Env::default();
        env.set(
            "print".to_string(),
            Value::BuiltinFn(BuiltinFn {
                ident: "print".to_string(),
                params: vec!["output".to_string()],
                body: |_env, args| {
                    let output = &args[0];
                    println!("{}", output);
                    Value::Unit
                },
            }),
        );
        Self { env }
    }
}

impl Visitor<ValueResult, InterpreterResult<()>> for Interpreter {
    fn visit_expr(&mut self, e: &SpanExpr) -> ValueResult {
        match &e.node {
            Expr::Literal(lit) => self.visit_literal(lit),
            Expr::Ident(ident) => self.visit_ident(ident, e.span),
            Expr::If {
                cond,
                true_value,
                false_value,
            } => self.visit_if(cond.as_ref(), true_value.as_ref(), false_value.as_ref()),
            Expr::Match { expr, arms } => self.visit_match(expr.as_ref(), arms),
            Expr::Block { exprs } => self.visit_block(exprs),
            Expr::BinaryOp { op, lhs, rhs } => {
                self.visit_binary_op(op, e.span, lhs.as_ref(), rhs.as_ref())
            }
            Expr::UnaryOp { op, expr } => self.visit_unary_op(e.span, op, expr.as_ref()),
            Expr::FnCall { fun, args } => self.visit_fncall(e.span, fun.as_ref(), args),
            Expr::Closure(_) => todo!(),
            Expr::Stmt(_) => todo!(),
        }
    }

    fn visit_stmt(&mut self, s: &SpanStmt) -> InterpreterResult<()> {
        match &s.node {
            Stmt::FnDef { ident, fun } => self.visit_fndef(ident, fun),
            Stmt::Let { ident, expr } => self.visit_let(ident, expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    Number,
    String,
    Function,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Unit => "unit".to_string(),
                Self::Bool => "bool".to_string(),
                Self::Number => "number".to_string(),
                Self::String => "string".to_string(),
                Self::Function => "function".to_string(),
            }
        )
    }
}
