use crate::{
    interpreter::{InterpreterError, Type, ValueResult},
    lexer::token::Span,
};

use super::Value;

impl Value {
    pub fn typ(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Function(_) => Type::Function,
            Value::BuiltinFn(_) => Type::Function,
            Value::Closure(_) => Type::Function,
        }
    }

    pub fn to_str(&self, span: Span) -> Result<&str, InterpreterError> {
        match self {
            Value::String(string) => Ok(string),
            _ => Err(InterpreterError::WrongType {
                span,
                expected: Type::String,
                got: self.typ(),
            }),
        }
    }

    pub fn to_number(&self, span: Span) -> Result<f64, InterpreterError> {
        match self {
            Value::Number(number) => Ok(*number),
            _ => Err(InterpreterError::WrongType {
                span,
                expected: Type::Number,
                got: self.typ(),
            }),
        }
    }

    pub fn add(&self, other: &Value, span: Span) -> ValueResult {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(s1.to_string() + s2)),
            (v1, v2) => Err(InterpreterError::CannotPerformOnType {
                span,
                op: "addition",
                lhs: v1.typ(),
                rhs: v2.typ(),
            }),
        }
    }

    pub fn sub(&self, other: &Value, span: Span) -> ValueResult {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
            (v1, v2) => Err(InterpreterError::CannotPerformOnType {
                span,
                op: "subtraction",
                lhs: v1.typ(),
                rhs: v2.typ(),
            }),
        }
    }

    pub fn mul(&self, other: &Value, span: Span) -> ValueResult {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
            (Value::String(s1), Value::Number(n1)) => Ok(Value::String(s1.repeat(*n1 as usize))),
            (v1, v2) => Err(InterpreterError::CannotPerformOnType {
                span,
                op: "multiplication",
                lhs: v1.typ(),
                rhs: v2.typ(),
            }),
        }
    }

    pub fn div(&self, other: &Value, span: Span) -> ValueResult {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
            (v1, v2) => Err(InterpreterError::CannotPerformOnType {
                span,
                op: "division",
                lhs: v1.typ(),
                rhs: v2.typ(),
            }),
        }
    }
}

impl From<Value> for bool {
    fn from(from: Value) -> Self {
        match from {
            Value::Unit => false,
            Value::Bool(b) => b,
            Value::Number(n) => n == 0_f64,
            Value::String(s) => !s.is_empty(),
            Value::Function(_) => true,
            Value::BuiltinFn(_) => true,
            Value::Closure(_) => true,
        }
    }
}

impl From<&Value> for bool {
    fn from(from: &Value) -> Self {
        match from {
            Value::Unit => false,
            Value::Bool(b) => *b,
            Value::Number(n) => *n == 0_f64,
            Value::String(s) => !s.is_empty(),
            Value::Function(_) => true,
            Value::BuiltinFn(_) => true,
            Value::Closure(_) => true,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(n2),
            (Value::String(s1), Value::String(s2)) => s1.partial_cmp(s2),
            _ => None,
        }
    }
}
