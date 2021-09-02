use crate::{
    ast::{Function, Stmt},
    lexer::token_kind::TokenKind,
};

use super::{error::SyntaxError, Parser, SyntaxResult};

#[allow(unused_must_use)]
impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> SyntaxResult<Stmt> {
        match self.peek() {
            TokenKind::Let => self.parse_let(),
            TokenKind::Function => self.parse_fndef(),
            TokenKind::Eof => Err(SyntaxError::UnexpectedEndOfInput(self.next_token()?)),

            _ => {
                let token = self.next_token()?;
                Err(SyntaxError::UnexpectedToken {
                    expected: "let or function".to_string(),
                    token,
                })
            }
        }
    }

    pub fn parse_let(&mut self) -> SyntaxResult<Stmt> {
        self.next_token();
        let ident = self.next_token()?;
        if !matches!(ident.kind, TokenKind::Ident) {
            return Err(SyntaxError::UnexpectedToken {
                expected: TokenKind::Ident.to_string(),
                token: ident,
            });
        }

        let ident = self.text(ident).to_string();
        self.consume(TokenKind::Assign)?;
        let expr = self.boxed_expr()?;

        Ok(Stmt::Let { ident, expr })
    }

    pub fn parse_fndef(&mut self) -> SyntaxResult<Stmt> {
        self.next_token();
        let ident = self.next_token()?;
        if !matches!(ident.kind, TokenKind::Ident) {
            return Err(SyntaxError::UnexpectedToken {
                expected: TokenKind::Ident.to_string(),
                token: ident,
            });
        }

        let ident = self.text(ident).to_string();
        let mut params = Vec::new();
        while self.at(TokenKind::Ident) {
            params.push({
                let ident = self.next_token()?;
                self.text(ident).to_string()
            });
        }
        self.consume(TokenKind::Assign)?;
        let body = self.boxed_expr()?;

        Ok(Stmt::FnDef {
            ident,
            fun: Function { params, body },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_stmt {
        ($sample:expr, $sexpr:expr) => {
            let test = $sample;
            match Parser::new(test).parse_stmt() {
                Ok(stmt) => {
                    println!(
                        "Sample: \"{}\"\nGot:    {}\nWanted: {}",
                        $sample, stmt, $sexpr
                    );
                    assert_eq!(stmt.to_string(), $sexpr)
                }
                Err(err) => {
                    err.display(test, "stmt_parsing_tests.ul");
                    assert!(false)
                }
            }
        };
    }

    #[test]
    fn parse_let() {
        assert_stmt!(
            "let thing = (57489 + 423) * 8989 - 9",
            "(let thing (- (* (+ 57489 423) 8989) 9))"
        );
    }

    #[test]
    fn parse_basic_fndef() {
        assert_stmt!(
            "function add x y = x + y",
            "(define add :params (x y) :body (+ x y))"
        );
    }

    #[test]
    fn parse_basic_fndef_no_params() {
        assert_stmt!(
            r#"function yes = print "yes""#,
            r#"(define yes :params () :body (print "yes"))"#
        );
    }

    #[test]
    fn parse_fndef_with_block() {
        assert_stmt!(
            r#"
function add x y = do
    let thing1 = x + 1;
    let thing2 = y + 2;
    thing1 + thing2
end"#,
            "(define add :params (x y) :body (block ((let thing1 (+ x 1)) (let thing2 (+ y 2)) (+ thing1 thing2))))"
        );
    }
}
