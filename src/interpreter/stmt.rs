use crate::{ast::Function, visitor::Visitor};

use super::{value::Value, Interpreter, InterpreterResult, SpanExpr};

impl Interpreter {
    pub(crate) fn visit_fndef(&mut self, ident: &str, fun: &Function) -> InterpreterResult<()> {
        self.env
            .set(ident.to_string(), Value::Function(fun.clone()));
        Ok(())
    }

    pub(crate) fn visit_let(&mut self, ident: &str, expr: &SpanExpr) -> InterpreterResult<()> {
        let value = self.visit_expr(expr)?;
        self.env.set(ident.to_string(), value);
        Ok(())
    }
}
