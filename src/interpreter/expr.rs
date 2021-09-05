use std::cmp::Ordering;

use crate::{
    ast::{Lit, MatchArm},
    interpreter::value::Value,
    lexer::{token::Span, token_kind::TokenKind},
    visitor::Visitor,
};

use super::{error::InterpreterError, Interpreter, SpanExpr, ValueResult};

impl Interpreter {
    pub(crate) fn visit_literal(&mut self, lit: &Lit) -> ValueResult {
        Ok(match lit {
            Lit::Unit => Value::Unit,
            Lit::Bool(b) => Value::Bool(*b),
            Lit::Int(i) => Value::Number(*i as f64),
            Lit::Float(f) => Value::Number(*f),
            Lit::String(s) => Value::String(s.clone()),
        })
    }

    pub(crate) fn visit_ident(&mut self, ident: &str, span: Span) -> ValueResult {
        self.env.get(ident, span)
    }

    pub(crate) fn visit_if(
        &mut self,
        cond: &SpanExpr,
        true_value: &SpanExpr,
        false_value: &SpanExpr,
    ) -> ValueResult {
        let cond: Value = self.visit_expr(cond)?;
        if cond.into() {
            self.visit_expr(true_value)
        } else {
            self.visit_expr(false_value)
        }
    }

    pub(crate) fn visit_match(&mut self, expr: &SpanExpr, arms: &[MatchArm]) -> ValueResult {
        let value: Value = self.visit_expr(expr)?;
        arms.iter()
            .find(|arm| {
                arm.pattern
                    .iter()
                    .any(|pattern| Ok(value.clone()) == self.visit_expr(pattern))
            })
            .ok_or(InterpreterError::UnexhaustiveMatch { span: expr.span })
            .and_then(|arm| self.visit_expr(arm.result.as_ref()))
    }

    pub(crate) fn visit_block(&mut self, exprs: &[SpanExpr]) -> ValueResult {
        let values = exprs.iter().map(|e| self.visit_expr(e)).collect::<Vec<_>>();
        for value in values.iter() {
            if let Err(e) = value {
                return Err(e.clone());
            }
        }
        values.last().map(Clone::clone).unwrap_or(Ok(Value::Unit))
    }

    pub(crate) fn visit_binary_op(
        &mut self,
        op: &TokenKind,
        span: Span,
        lhs: &SpanExpr,
        rhs: &SpanExpr,
    ) -> ValueResult {
        let lhs = self.visit_expr(lhs)?;
        match op {
            TokenKind::Less
            | TokenKind::Greater
            | TokenKind::LessEq
            | TokenKind::GreatEq
            | TokenKind::Equals
            | TokenKind::NotEq => {
                let rhs = self.visit_expr(rhs)?;
                self.visit_comparison(op, span, &lhs, &rhs)
            }
            TokenKind::Add | TokenKind::Minus | TokenKind::Multiply | TokenKind::Divide => {
                let rhs = self.visit_expr(rhs)?;
                self.visit_arithmetic_op(span, op, &lhs, &rhs)
            }
            TokenKind::And | TokenKind::Or => self.visit_short_circuiting_op(op, &lhs, rhs),
            _ => unreachable!(),
        }
    }

    pub(crate) fn visit_comparison(
        &mut self,
        op: &TokenKind,
        span: Span,
        lhs: &Value,
        rhs: &Value,
    ) -> ValueResult {
        let ordering = lhs
            .partial_cmp(rhs)
            .ok_or(InterpreterError::CannotCompare {
                span,
                lhs: lhs.typ(),
                rhs: rhs.typ(),
            })?;

        Ok(Value::Bool(matches!(
            (op, ordering),
            (TokenKind::Less | TokenKind::LessEq, Ordering::Less)
                | (TokenKind::Greater | TokenKind::GreatEq, Ordering::Greater)
                | (
                    TokenKind::Equals | TokenKind::LessEq | TokenKind::GreatEq,
                    Ordering::Equal
                )
                | (TokenKind::NotEq, Ordering::Less | Ordering::Greater)
        )))
    }

    pub(crate) fn visit_arithmetic_op(
        &mut self,
        span: Span,
        op: &TokenKind,
        lhs: &Value,
        rhs: &Value,
    ) -> ValueResult {
        match op {
            TokenKind::Add => lhs.add(rhs, span),
            TokenKind::Minus => lhs.sub(rhs, span),
            TokenKind::Multiply => lhs.mul(rhs, span),
            TokenKind::Divide => lhs.div(rhs, span),
            _ => unreachable!(),
        }
    }

    pub(crate) fn visit_short_circuiting_op(
        &mut self,
        op: &TokenKind,
        lhs: &Value,
        rhs: &SpanExpr,
    ) -> ValueResult {
        Ok(Value::Bool(match (op, bool::from(lhs)) {
            (TokenKind::And, false) => false,
            (TokenKind::And, true) => bool::from(self.visit_expr(rhs)?),
            (TokenKind::Or, true) => true,
            (TokenKind::Or, false) => bool::from(self.visit_expr(rhs)?),
            _ => unreachable!(),
        }))
    }

    pub(crate) fn visit_unary_op(
        &mut self,
        span: Span,
        op: &TokenKind,
        expr: &SpanExpr,
    ) -> ValueResult {
        let value = self.visit_expr(expr)?;
        Ok(match op {
            TokenKind::Not => Value::Bool(!bool::from(value)),
            TokenKind::Minus => Value::Number(-value.to_number(span)?),
            TokenKind::Question => unimplemented!(),
            _ => unreachable!(),
        })
    }

    pub(crate) fn visit_fncall(
        &mut self,
        span: Span,
        fun: &SpanExpr,
        args: &[SpanExpr],
    ) -> ValueResult {
        let value = match self.visit_expr(fun)? {
            Value::Function(fun) => {
                let params_len = fun.params.len();
                let args_len = args.len();
                if params_len != args_len {
                    return Err(InterpreterError::ArityMismatch {
                        span,
                        expected: params_len,
                        got: args_len,
                    });
                }

                self.env.new_scope();
                for (param, arg) in fun.params.iter().zip(args) {
                    let value = self.visit_expr(arg)?;
                    self.env.set(param.to_string(), value);
                }
                self.visit_expr(fun.body.as_ref())?
            }
            Value::BuiltinFn(fun) => {
                let params_len = fun.params.len();
                let args_len = args.len();
                if params_len != args_len {
                    return Err(InterpreterError::ArityMismatch {
                        span,
                        expected: params_len,
                        got: args_len,
                    });
                }

                let mut args_values = Vec::new();
                for arg in args {
                    args_values.push(self.visit_expr(arg)?);
                }
                self.env.new_scope();
                (fun.body)(&mut self.env, &args_values)
            }
            v => return Err(InterpreterError::NotCallable { span, typ: v.typ() }),
        };

        self.env.exit_scope();
        Ok(value)
    }
}
