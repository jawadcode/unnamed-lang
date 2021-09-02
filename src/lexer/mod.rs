use logos::Logos;

use self::{
    token::Token,
    token_kind::{LogosToken, TokenKind},
};

pub mod token;
pub mod token_kind;

/// Wrapper around `logos::SpannedIter` that is an `Iterator` over the `Token` struct
pub struct Lexer<'input> {
    input: &'input str,
    generated: logos::SpannedIter<'input, LogosToken>,
    eof: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            generated: LogosToken::lexer(input).spanned(),
            eof: false,
        }
    }

    /// Checkmate 'Muricans
    pub fn tokenise(&mut self) -> Vec<Token> {
        self.collect()
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    /// Wrapper around `logos::SpannedIter::next` that transforms the span + token kind into our custom `Token` object
    fn next(&mut self) -> Option<Self::Item> {
        match self.generated.next() {
            Some((token, span)) => Some(Token {
                kind: token.into(),
                span: span.into(),
            }),
            None if self.eof => None,
            None => {
                let len = self.input.len();
                self.eof = true;
                Some(Token {
                    kind: TokenKind::Eof,
                    span: (len..len).into(),
                })
            }
        }
    }
}
