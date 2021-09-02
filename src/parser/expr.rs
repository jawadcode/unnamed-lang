use crate::{
    ast::{Expr, Function, Lit, MatchArm},
    lexer::token_kind::TokenKind,
    parser::Spanned,
};

use super::{error::SyntaxError, ParseResult, Parser, SyntaxResult};

// const EXPRESSION_TERMINATOR: [TokenKind; 10] = [
//     TokenKind::RightParen,
//     TokenKind::FatArrow,
//     TokenKind::Question,
//     TokenKind::Semicolon,
//     TokenKind::Comma,
//     TokenKind::Pipe,
//     TokenKind::Then,
//     TokenKind::Else,
//     TokenKind::End,
//     TokenKind::Eof,
// ];

const FUNCTION_ARG_TOKENS: [TokenKind; 8] = [
    TokenKind::Unit,
    TokenKind::True,
    TokenKind::False,
    TokenKind::IntLit,
    TokenKind::FloatLit,
    TokenKind::StringLit,
    TokenKind::LeftParen,
    TokenKind::Ident,
];

const BASIC_EXPR_TOKENS: [TokenKind; 7] = [
    TokenKind::Unit,
    TokenKind::True,
    TokenKind::False,
    TokenKind::IntLit,
    TokenKind::FloatLit,
    TokenKind::StringLit,
    TokenKind::Ident,
];

/* Credit to https://domenicquirl.github.io/blog/parsing-basics/#binary-operators */
trait Operator {
    /// Prefix operators bind their operand to the right
    fn prefix_binding_power(&self) -> Option<((), u8)>;

    /// Infix operators bind two operands, lhs and rhs
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left
    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> Option<((), u8)> {
        Some(match self {
            TokenKind::Minus => ((), 51),
            TokenKind::Not => ((), 101),
            _ => {
                return None;
            }
        })
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        Some(match self {
            TokenKind::Or => (1, 2),
            TokenKind::And => (3, 4),
            TokenKind::Equals | TokenKind::NotEq => (5, 6),
            TokenKind::Less | TokenKind::Greater | TokenKind::LessEq | TokenKind::GreatEq => (7, 8),
            TokenKind::Plus | TokenKind::Minus => (9, 10),
            TokenKind::Multiply | TokenKind::Divide => (11, 12),
            _ => return None,
        })
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        Some(match self {
            TokenKind::Question => (101, ()),
            _ => return None,
        })
    }
}

#[allow(unused_must_use)]
impl Parser<'_> {
    fn parse_expr(&mut self, binding_power: u8) -> ParseResult<Expr> {
        let mut lhs = match self.peek() {
            lit @ TokenKind::Unit
            | lit @ TokenKind::True
            | lit @ TokenKind::False
            | lit @ TokenKind::IntLit
            | lit @ TokenKind::FloatLit
            | lit @ TokenKind::StringLit => self.parse_lit(lit)?,

            TokenKind::Ident => self.parse_ident()?,
            TokenKind::Fn => self.parse_closure()?,
            TokenKind::If => self.parse_if_expr()?,
            TokenKind::Match => self.parse_match_expr()?,
            TokenKind::Do => self.parse_block_expr()?,
            TokenKind::LeftParen => self.parse_grouping()?,

            op @ TokenKind::Minus | op @ TokenKind::Not => self.parse_prefix_op(op)?,
            _ => {
                let token = self.next_token()?;
                return Err(SyntaxError::UnexpectedToken {
                    expected: "expression".to_string(),
                    token,
                });
            }
        };

        if self.multi_at(&FUNCTION_ARG_TOKENS) {
            lhs = self.parse_fn_call(lhs)?;
        }

        /* Credit to https://domenicquirl.github.io/blog/parsing-basics/#binary-operators */
        loop {
            let op = match self.peek() {
                op @ TokenKind::Plus
                | op @ TokenKind::Minus
                | op @ TokenKind::Multiply
                | op @ TokenKind::Divide
                | op @ TokenKind::And
                | op @ TokenKind::Or
                | op @ TokenKind::Less
                | op @ TokenKind::Greater
                | op @ TokenKind::Not
                | op @ TokenKind::LessEq
                | op @ TokenKind::GreatEq
                | op @ TokenKind::NotEq
                | op @ TokenKind::Equals => op,
                // These tokens are expression terminators,
                // i.e. if one of these appears after an expression, you stop parsing it
                TokenKind::RightParen
                | TokenKind::FatArrow
                | TokenKind::Question
                | TokenKind::Semicolon
                | TokenKind::Comma
                | TokenKind::Pipe
                | TokenKind::Then
                | TokenKind::Else
                | TokenKind::End
                | TokenKind::Eof => break,

                _ => {
                    let token = self.next_token()?;
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "operator or expression terminator".to_string(),
                        token,
                    });
                }
            };

            if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                let op_token = self.consume_next(op)?;

                // no recursive call here, because we have already
                // parsed our operand `lhs`
                lhs = Spanned {
                    span: (lhs.span.start..op_token.span.end).into(),
                    node: Expr::UnaryOp {
                        op,
                        expr: Box::new(lhs),
                    },
                };
                // parsed an operator --> go round the loop again
                continue;
            }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                self.consume(op)?;

                let rhs = self.parse_expr(right_binding_power)?;
                lhs = Spanned {
                    span: (lhs.span.start..rhs.span.end).into(),
                    node: Expr::BinaryOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                };
                // parsed an operator --> go round the loop again
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    #[inline(always)]
    fn parse_stmt_expr(&mut self) -> ParseResult<Expr> {
        let stmt = self.parse_stmt()?;
        Ok(Spanned {
            span: stmt.span,
            node: Expr::Stmt(stmt),
        })
    }

    fn parse_basic_expr(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            lit @ TokenKind::Unit
            | lit @ TokenKind::True
            | lit @ TokenKind::False
            | lit @ TokenKind::IntLit
            | lit @ TokenKind::FloatLit
            | lit @ TokenKind::StringLit => self.parse_lit(lit),
            TokenKind::Ident => self.parse_ident(),

            _ => {
                let token = self.next_token()?;
                Err(SyntaxError::UnexpectedToken {
                    expected: "literal or identifier".to_string(),
                    token,
                })
            }
        }
    }

    fn parse_lit(&mut self, lit: TokenKind) -> ParseResult<Expr> {
        let token = self.next_token()?;
        let text = self.text(token);

        Ok(Spanned {
            span: token.span,
            node: Expr::Literal(match lit {
                TokenKind::Unit => Lit::Unit,
                TokenKind::True => Lit::Bool(true),
                TokenKind::False => Lit::Bool(false),
                TokenKind::IntLit => Lit::Int(
                    // stdlib ftw
                    text.parse()
                        .map_err(|_| SyntaxError::InvalidLiteral(token))?,
                ),
                TokenKind::FloatLit => Lit::Float(
                    text.parse()
                        .map_err(|_| SyntaxError::InvalidLiteral(token))?,
                ),
                TokenKind::StringLit => Lit::String(
                    // Trim the quotes
                    text[1..(text.len() - 1)].to_string(),
                ),
                _ => unreachable!(),
            }),
        })
    }

    fn parse_ident(&mut self) -> ParseResult<Expr> {
        let token = self.next_token()?;
        let text = self.text(token);

        Ok(Spanned {
            span: token.span,
            node: Expr::Ident(text.to_string()),
        })
    }

    fn parse_closure(&mut self) -> ParseResult<Expr> {
        let token = self.next_token()?;
        let mut params = Vec::new();
        while self.at(TokenKind::Ident) {
            let text = {
                let token = self.next_token()?;
                self.text(token)
            };
            params.push(text.to_string());
        }
        self.consume(TokenKind::FatArrow)?;

        let body = self.boxed_expr()?;
        Ok(Spanned {
            span: (token.span.start..body.span.end).into(),
            node: Expr::Closure(Function { params, body }),
        })
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let token = self.next_token()?;
        let cond = self.boxed_expr()?;
        self.consume(TokenKind::Then)?;
        let true_value = self.boxed_expr()?;
        self.consume(TokenKind::Else)?;
        let false_value = self.boxed_expr()?;

        Ok(Spanned {
            span: (token.span.start..false_value.span.end).into(),
            node: Expr::If {
                cond,
                true_value,
                false_value,
            },
        })
    }

    fn parse_match_expr(&mut self) -> ParseResult<Expr> {
        let token = self.next_token()?;
        let expr = self.boxed_expr()?;
        let mut arms = Vec::new();
        // Parse all match arms
        while self.at(TokenKind::Pipe) {
            self.next_token()?;

            let mut pattern = Vec::new();
            // Parse all match arm patterns
            while self.multi_at(&BASIC_EXPR_TOKENS) {
                pattern.push(self.parse_basic_expr()?);
                if self.at(TokenKind::Comma) {
                    self.consume(TokenKind::Comma);
                } else {
                    break;
                }
            }

            self.consume(TokenKind::FatArrow)?;
            arms.push(MatchArm {
                pattern,
                result: self.boxed_expr()?,
            });
        }

        let end = self.consume_next(TokenKind::End)?;
        Ok(Spanned {
            span: (token.span.start..end.span.end).into(),
            node: Expr::Match { expr, arms },
        })
    }

    fn parse_block_expr(&mut self) -> ParseResult<Expr> {
        let token = self.next_token()?;
        let mut exprs = Vec::new();
        while !self.at(TokenKind::End) {
            if self.multi_at(&[TokenKind::Let, TokenKind::Function]) {
                exprs.push(self.parse_stmt_expr()?)
            } else {
                exprs.push(self.expr()?);
            }
            if !self.at(TokenKind::Semicolon) {
                break;
            }
            self.consume(TokenKind::Semicolon)?;
        }

        let end = self.consume_next(TokenKind::End)?;
        Ok(Spanned {
            span: (token.span.start..end.span.end).into(),
            node: Expr::Block { exprs },
        })
    }

    fn parse_grouping(&mut self) -> ParseResult<Expr> {
        let token = self.next_token()?;
        let expr = self.expr()?.node;
        let end = self.consume_next(TokenKind::RightParen)?;
        Ok(Spanned {
            span: (token.span.start..end.span.end).into(),
            node: expr,
        })
    }

    fn parse_fn_call(&mut self, lhs: Spanned<Expr>) -> ParseResult<Expr> {
        let mut args = Vec::new();
        while self.multi_at(&FUNCTION_ARG_TOKENS) {
            args.push(match self.peek() {
                lit @ TokenKind::Unit
                | lit @ TokenKind::True
                | lit @ TokenKind::False
                | lit @ TokenKind::IntLit
                | lit @ TokenKind::FloatLit
                | lit @ TokenKind::StringLit => self.parse_lit(lit)?,
                TokenKind::Ident => self.parse_ident()?,
                TokenKind::LeftParen => self.parse_grouping()?,

                _ => {
                    let token = self.next_token()?;
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "function argument, operation or expression terminator"
                            .to_string(),
                        token,
                    });
                }
            })
        }
        Ok(if args.is_empty() {
            lhs
        } else {
            // We can unwrap because `args` is guaranteed to have at least one element here
            let end = args.last().unwrap();
            Spanned {
                span: (lhs.span.start..end.span.end).into(),
                node: Expr::FnCall {
                    fun: Box::new(lhs),
                    args,
                },
            }
        })
    }

    fn parse_prefix_op(&mut self, op: TokenKind) -> ParseResult<Expr> {
        let token = self.next_token()?;
        // Get right binding power of the operator,
        // we can unwrap because `op` is guaranteed to be a valid prefix operator
        // because of where it is called in `self.parse_expr`
        let ((), right_binding_power) = op.prefix_binding_power().unwrap();

        let expr = Box::new(self.parse_expr(right_binding_power)?);
        Ok(Spanned {
            span: (token.span.start..expr.span.end).into(),
            node: Expr::UnaryOp { op, expr },
        })
    }

    #[inline(always)]
    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.parse_expr(0)
    }

    #[inline(always)]
    pub(crate) fn boxed_expr(&mut self) -> SyntaxResult<Box<Spanned<Expr>>> {
        self.parse_expr(0).map(Box::new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_expr {
        ($sample:expr, $sexpr:expr) => {
            let test = $sample;
            println!("Sample: '{}'", $sample);
            match Parser::new(test).expr() {
                Ok(expr) => {
                    println!(
                        "Sample: \"{}\"\nGot:    {}\nWanted: {}\n",
                        $sample, expr, $sexpr
                    );
                    assert_eq!(expr.to_string(), $sexpr);
                }
                Err(err) => {
                    err.display(test, "expr_parsing_tests.ul");
                    assert!(false)
                }
            }
        };
    }

    #[test]
    fn parse_bool_expr() {
        assert_expr!(
            "!true or 10 < 6 and 2 != 1",
            "(or (! true) (and (< 10 6) (!= 2 1)))"
        );
    }

    #[test]
    fn parse_arithmetic_comparison_operators() {
        assert_expr!(
            "10 + -9 * 0 - 90 / -90 != 69 / -4",
            "(!= (- (+ 10 (* (- 9) 0)) (/ 90 (- 90))) (/ 69 (- 4)))"
        );
    }

    #[test]
    fn parse_identifier() {
        assert_expr!("hello", "hello");
    }

    #[test]
    fn parse_simple_function_call() {
        assert_expr!("add 1 (69 + 420) 2", "(add 1 (+ 69 420) 2)");
    }

    #[test]
    fn parse_nested_function_call() {
        assert_expr!(
            "add (69 + 420) (add 57893 43280)",
            "(add (+ 69 420) (add 57893 43280))"
        );
    }

    #[test]
    fn parse_if_expr() {
        assert_expr!(
            "if 10 > 89 + 90 then 1 + 1 else -1",
            "(if :cond (> 10 (+ 89 90)) :then (+ 1 1) :else (- 1))"
        );
    }

    #[test]
    fn parse_match_expr() {
        assert_expr!(
            "
match x
  | 1    => 69
  | 2, 3 => 420
  | _    => x * x * x
end",
            "(match :target x :arms (((1) 69) ((2 3) 420) ((_) (* (* x x) x))))"
        );
    }

    #[test]
    fn parse_closure() {
        assert_expr!("fn x => x + 1", "(lambda :params (x) :body (+ x 1))");
    }

    #[test]
    fn parse_block_expr() {
        assert_expr!(
            r#"
do
  let thing = 123;
  print "lol";
  let thing2 = 234;
  thing + thing2
end"#,
            r#"(block ((let thing 123) (print "lol") (let thing2 234) (+ thing thing2)))"#
        );
    }
}
