use crate::{
    ast::{Function, Stmt},
    lexer::token_kind::TokenKind,
};

use super::{error::SyntaxError, ParseResult, Parser, Spanned};

#[allow(unused_must_use)]
impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek() {
            TokenKind::Let => self.parse_let(),
            TokenKind::Function => self.parse_fndef(),
            TokenKind::Eof => Err(SyntaxError::End),

            _ => {
                let token = self.next_token()?;
                Err(SyntaxError::UnexpectedToken {
                    expected: "let or function".to_string(),
                    token,
                })
            }
        }
    }

    fn parse_let(&mut self) -> ParseResult<Stmt> {
        self.next_token();
        let ident = self.consume_next(TokenKind::Ident)?;

        let text = self.text(ident).to_string();
        self.consume(TokenKind::Assign)?;
        let expr = self.boxed_expr()?;

        Ok(Spanned {
            span: (ident.span.start..expr.span.end).into(),
            node: Stmt::Let { ident: text, expr },
        })
    }

    fn parse_fndef(&mut self) -> ParseResult<Stmt> {
        self.next_token();
        let ident = self.consume_next(TokenKind::Ident)?;

        let text = self.text(ident);
        let mut params = Vec::new();
        while self.at(TokenKind::Ident) {
            params.push({
                let ident = self.next_token()?;
                self.text(ident).to_string()
            });
        }
        self.consume(TokenKind::Assign)?;
        let body = self.boxed_expr()?;

        Ok(Spanned {
            span: (ident.span.start..body.span.end).into(),
            node: Stmt::FnDef {
                ident: text.to_string(),
                fun: Function {
                    ident: Some(text.to_string()),
                    params,
                    body,
                },
            },
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
            r#"function yes = print("yes")"#,
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
