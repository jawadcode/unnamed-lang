use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

use crate::lexer::token::Span;

use super::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterError {
    Undefined {
        span: Span,
        ident: String,
    },
    WrongType {
        span: Span,
        expected: Type,
        got: Type,
    },
    UnexhaustiveMatch {
        span: Span,
    },
    CannotPerformOnType {
        span: Span,
        lhs: Type,
        rhs: Type,
        op: &'static str,
    },
    CannotCompare {
        span: Span,
        lhs: Type,
        rhs: Type,
    },
    NotCallable {
        span: Span,
        typ: Type,
    },
    ArityMismatch {
        span: Span,
        expected: usize,
        got: usize,
    },
    MissingMain,
}

impl InterpreterError {
    pub fn display(&self, input: &str, file_id: &str) {
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_id, input);
        let (msg, span) = match self {
            Self::Undefined { span, ident } => {
                (format!("Value error: '{}' is undefined", ident), *span)
            }
            Self::WrongType {
                span,
                expected,
                got,
            } => (
                format!("Type error: Expected type '{}', got type {}", expected, got),
                *span,
            ),
            Self::UnexhaustiveMatch { span } => ("Unexhaustive match".to_string(), *span),
            Self::CannotPerformOnType { span, lhs, rhs, op } => (
                format!("Type error: Cannot perform {} on {} and {}", op, lhs, rhs),
                *span,
            ),
            Self::CannotCompare { span, lhs, rhs } => (
                format!("Type error: Cannot compare {} and {}", lhs, rhs),
                *span,
            ),
            Self::NotCallable { span, typ } => {
                (format!("Type error: {} is not callable", typ), *span)
            }
            Self::ArityMismatch {
                span,
                expected,
                got,
            } => (
                format!("Expected {} function call arguments, got {}", expected, got),
                *span,
            ),
            Self::MissingMain => {
                let diagnostic = Diagnostic::error().with_message("Missing 'main' function");
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = Config::default();

                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                return;
            }
        };

        let diagnostic = Diagnostic::error()
            .with_message(&msg)
            .with_labels(vec![Label::primary(file_id, span).with_message(&msg)]);
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();

        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }
}
