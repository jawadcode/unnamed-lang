pub mod error;
pub mod expr;
pub mod stmt;

use std::iter::Peekable;

use crate::lexer::{token::Token, token_kind::TokenKind, Lexer};

use self::error::SyntaxError;

type SyntaxResult<T> = Result<T, SyntaxError>;

pub struct Parser<'input> {
    input: &'input str,
    lexer: Peekable<Lexer<'input>>,
}

impl<'input> Parser<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            lexer: Lexer::new(input).peekable(),
        }
    }

    /// Get next token, returning an `UnexpectedEndOfInput` if the lexer returns `None`
    pub fn next_token(&mut self) -> SyntaxResult<Token> {
        self.lexer.next().ok_or_else(|| {
            SyntaxError::UnexpectedEndOfInput(Token {
                kind: TokenKind::Eof,
                span: (self.input.len()..self.input.len()).into(),
            })
        })
    }

    #[inline(always)]
    /// Get the source text of a given token
    pub fn text(&self, token: Token) -> &'input str {
        token.text(self.input)
    }

    #[inline(always)]
    /// Look ahead to the next token without consuming it
    pub fn peek(&mut self) -> TokenKind {
        self.lexer
            .peek()
            .map(|token| token.kind)
            .unwrap_or(TokenKind::Eof)
    }

    #[inline(always)]
    /// Peek ahead to the next token and check if its `TokenKind` is `kind`
    pub fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    #[inline(always)]
    /// `self.at` but for multiple tokens
    pub fn multi_at(&mut self, kinds: &'static [TokenKind]) -> bool {
        kinds.contains(&self.peek())
    }

    /// Consume token and check that it's `TokenKind` is as `expected`
    pub fn consume(&mut self, expected: TokenKind) -> SyntaxResult<()> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                token,
            })
        } else {
            Ok(())
        }
    }
}
