use crate::lexer::token::Token;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

pub enum SyntaxError {
    UnexpectedToken { expected: String, token: Token },
    InvalidLiteral(Token),
    UnexpectedEndOfInput(Token),
    InvalidToken(Token),
}

impl SyntaxError {
    pub fn display(&self, input: &str, file_id: &str) {
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_id, input);
        let diagnostic = match self {
            SyntaxError::UnexpectedToken { expected, token } => Diagnostic::error()
                .with_message(format!(
                    "Unexpected token: Expected {}, got {}",
                    expected, token.kind,
                ))
                .with_labels(vec![Label::primary(file_id, token.span)
                    .with_message(format!("Expected {}, got {}", expected, token.kind))]),
            SyntaxError::InvalidLiteral(token) => Diagnostic::error()
                .with_message(format!("Invalid {}: '{}'", token.kind, token.text(input)))
                .with_labels(vec![Label::primary(file_id, token.span)
                    .with_message(format!("Invalid {}", token.kind))]),
            SyntaxError::UnexpectedEndOfInput(token) => Diagnostic::error()
                .with_message("Unexpected end of input".to_string())
                .with_labels(vec![
                    Label::primary(file_id, token.span).with_message("Unexpected end of input")
                ]),
            SyntaxError::InvalidToken(token) => Diagnostic::error()
                .with_message(token.kind.to_string())
                .with_labels(vec![
                    Label::primary(file_id, token.span).with_message(token.kind.to_string())
                ]),
        };

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();

        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{
        token::{Span, Token},
        token_kind::TokenKind,
    };

    use super::SyntaxError;

    #[test]
    fn unexpected_token() {
        let error = SyntaxError::UnexpectedToken {
            expected: "operator".to_string(),
            token: Token {
                kind: TokenKind::IntLit,
                span: Span { start: 2, end: 3 },
            },
        };
        let input = "1 2";
        error.display(input, "testing1.ul");
    }

    #[test]
    fn invalid_literal() {
        let error = SyntaxError::InvalidLiteral(Token {
            kind: TokenKind::IntLit,
            span: Span { start: 0, end: 61 },
        });
        let input = "1567384956783956738925673892567238956723895672389562895647389";
        error.display(input, "testing2.ul");
    }

    #[test]
    fn unexpected_end_of_input() {
        let error = SyntaxError::UnexpectedEndOfInput(Token {
            kind: TokenKind::Eof,
            span: Span { start: 2, end: 2 },
        });
        let input = "12";
        error.display(input, "testing3.ul");
    }

    #[test]
    fn invalid_token() {
        let error = SyntaxError::InvalidToken(Token {
            kind: TokenKind::Error,
            span: Span { start: 0, end: 12 },
        });
        let input = r#"%aaaaaaaaaaa"#;
        error.display(input, "testing4.ul")
    }
}
