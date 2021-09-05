use std::fmt;

use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    /* KEYWORDS */
    Function,
    Let,
    Fn,
    Struct,
    Enum,
    If,
    Then,
    Else,
    Match,
    Do,
    End,
    /* LITERALS */
    Ident,
    Unit,
    True,
    False,
    IntLit,
    FloatLit,
    StringLit,
    /* ARITHMETIC OPERATORS */
    Add,
    Minus,
    Multiply,
    Divide,
    /* COMPARISON OPERATORS */
    Less,
    Greater,
    Not,
    And,
    Or,
    LessEq,
    GreatEq,
    NotEq,
    Equals,
    /* BRACKETS */
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,
    /* MISC */
    Assign,
    FnPipe,
    Colon,
    Pipe,
    Comma,
    FatArrow,
    Question,
    Semicolon,
    Eof,
    Error,
}

#[derive(Debug, Clone, Logos)]
pub enum LogosToken {
    #[token("function")]
    Function,

    #[token("let")]
    Let,

    #[token("fn")]
    Fn,

    #[token("struct")]
    Struct,

    #[token("enum")]
    Enum,

    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    #[token("match")]
    Match,

    #[token("do")]
    Do,

    #[token("end")]
    End,

    #[regex(r#"([A-Za-z]|_)([A-Za-z]|_|\d)*"#)]
    Ident,

    #[token("()")]
    Unit,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[regex("[0-9]+", priority = 2)]
    IntLit,

    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    FloatLit,

    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    StringLit,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("!")]
    Bang,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("<=")]
    LessEq,

    #[token(">=")]
    GreaterEq,

    #[token("!=")]
    NotEq,

    #[token("==")]
    Equals,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("[")]
    LeftSquare,

    #[token("]")]
    RightSquare,

    #[token("{")]
    LeftCurly,

    #[token("}")]
    RightCurly,

    #[token("=")]
    Assign,

    #[token("|>")]
    FnPipe,

    #[token(":")]
    Colon,

    #[token("|")]
    Pipe,

    #[token("=>")]
    FatArrow,

    #[token(",")]
    Comma,

    #[token("?")]
    Question,

    #[token(";")]
    Semicolon,

    #[regex(r"\(\*([^*]|\*[^/])*\*\)", logos::skip)]
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[error]
    Error,
}

impl From<LogosToken> for TokenKind {
    fn from(from: LogosToken) -> Self {
        match from {
            LogosToken::Function => Self::Function,
            LogosToken::Let => Self::Let,
            LogosToken::Fn => Self::Fn,
            LogosToken::Struct => Self::Struct,
            LogosToken::Enum => Self::Enum,
            LogosToken::If => Self::If,
            LogosToken::Then => Self::Then,
            LogosToken::Else => Self::Else,
            LogosToken::Match => Self::Match,
            LogosToken::Do => Self::Do,
            LogosToken::End => Self::End,
            LogosToken::Ident => Self::Ident,
            LogosToken::Unit => Self::Unit,
            LogosToken::True => Self::True,
            LogosToken::False => Self::False,
            LogosToken::IntLit => Self::IntLit,
            LogosToken::FloatLit => Self::FloatLit,
            LogosToken::StringLit => Self::StringLit,
            LogosToken::Plus => Self::Add,
            LogosToken::Minus => Self::Minus,
            LogosToken::Multiply => Self::Multiply,
            LogosToken::Divide => Self::Divide,
            LogosToken::Less => Self::Less,
            LogosToken::Greater => Self::Greater,
            LogosToken::Bang => Self::Not,
            LogosToken::And => Self::And,
            LogosToken::Or => Self::Or,
            LogosToken::LessEq => Self::LessEq,
            LogosToken::GreaterEq => Self::GreatEq,
            LogosToken::NotEq => Self::NotEq,
            LogosToken::Equals => Self::Equals,
            LogosToken::LeftParen => Self::LeftParen,
            LogosToken::RightParen => Self::RightParen,
            LogosToken::LeftSquare => Self::LeftSquare,
            LogosToken::RightSquare => Self::RightSquare,
            LogosToken::LeftCurly => Self::LeftCurly,
            LogosToken::RightCurly => Self::RightCurly,
            LogosToken::Assign => Self::Assign,
            LogosToken::FnPipe => Self::FnPipe,
            LogosToken::Colon => Self::Colon,
            LogosToken::Pipe => Self::Pipe,
            LogosToken::Comma => Self::Comma,
            LogosToken::FatArrow => Self::FatArrow,
            LogosToken::Question => Self::Question,
            LogosToken::Semicolon => Self::Semicolon,
            LogosToken::Error => Self::Error,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Function => "function",
                Self::Let => "let",
                Self::Fn => "fn",
                Self::Struct => "struct",
                Self::Enum => "enum",
                Self::If => "if",
                Self::Then => "then",
                Self::Else => "else",
                Self::Match => "match",
                Self::Do => "do",
                Self::End => "end",
                Self::Ident => "identifier",
                Self::Unit => "unit",
                Self::True => "true",
                Self::False => "false",
                Self::IntLit => "integer literal",
                Self::FloatLit => "float literal",
                Self::StringLit => "string literal",
                Self::Add => "+",
                Self::Minus => "-",
                Self::Multiply => "*",
                Self::Divide => "/",
                Self::Less => "<",
                Self::Greater => ">",
                Self::Not => "!",
                Self::And => "and",
                Self::Or => "or",
                Self::LessEq => "==",
                Self::GreatEq => ">=",
                Self::NotEq => "!=",
                Self::Equals => "==",
                Self::LeftParen => "(",
                Self::RightParen => ")",
                Self::LeftSquare => "[",
                Self::RightSquare => "]",
                Self::LeftCurly => "{",
                Self::RightCurly => "}",
                Self::Assign => "=",
                Self::FnPipe => "|>",
                Self::Colon => ":",
                Self::Pipe => "|",
                Self::Comma => ",",
                Self::FatArrow => "=>",
                Self::Question => "?",
                Self::Semicolon => ";",
                Self::Eof => "EOF",
                Self::Error => "invalid token",
            }
        )
    }
}
