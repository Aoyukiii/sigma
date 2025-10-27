use crate::core::{
    diagnostics::errors::ParseErrorKind,
    syntax::{
        ast::raw::{
            Expr, ExprKind,
            expr::{Annotated, Application, InfixExpr, Lambda, Let, PrefixExpr},
            operator::{Infix, Prefix},
        },
        lexer::{
            stream::TokenStream,
            token::{Token, TokenKind},
        },
        parser::Parser,
    },
};

impl<'a, T> Parser<'a, T>
where
    T: TokenStream<'a>,
{
    pub fn expr(&mut self) -> Expr {
        let expr = self.expr_bp(0);
        if self.tokens.peek_is(TokenKind::RParen) {
            // error recovery for `)`
            let span = self.tokens.next().span;
            self.report((ParseErrorKind::UnclosedRParen, span).into());
            return self.expr_bp_with_lhs(0, expr);
        }
        expr
    }

    pub fn expr_bp(&mut self, min_bp: u8) -> Expr {
        if self.tokens.peek_is(TokenKind::RParen) {
            let span = self.tokens.peek().span;
            self.report(
                (
                    ParseErrorKind::new_expected_expr(TokenKind::RParen.to_string()),
                    span,
                )
                    .into(),
            );
            return Expr::new_err(span);
        }

        let Token { kind: next, span } = self.tokens.next();
        let lhs = match next {
            Ok(TokenKind::Atom(it)) => (ExprKind::AtomLiteral(it.to_string()), span).into(),
            Ok(TokenKind::Ident(it)) => (ExprKind::Ident(it.to_string()), span).into(),
            Ok(TokenKind::KwAtom) => (ExprKind::Atom, span).into(),
            Ok(TokenKind::KwType) => (ExprKind::Type, span).into(),
            Ok(TokenKind::Plus) | Ok(TokenKind::Minus) | Ok(TokenKind::Not) => {
                let op = match next.unwrap() {
                    TokenKind::Plus => Prefix::Plus,
                    TokenKind::Minus => Prefix::Neg,
                    TokenKind::Not => Prefix::Not,
                    _ => unreachable!(),
                };
                let r_bp = op.binding_power();
                let op_span = span;
                let rhs = self.expr_bp(r_bp);
                let span = op_span.merge(rhs.span);
                (
                    ExprKind::Prefix(Box::new(PrefixExpr {
                        op,
                        op_span,
                        rhs: Box::new(rhs),
                    })),
                    span,
                )
                    .into()
            }
            Ok(TokenKind::LParen) => {
                let syntax = self.expr_bp(0);
                if !self.tokens.expect(TokenKind::RParen) {
                    self.report((ParseErrorKind::UnclosedLParen, span).into());
                }
                syntax
            }
            Ok(TokenKind::RParen) => unreachable!(),
            Ok(TokenKind::KwLet) => {
                let var = Box::new(self.expr());
                self.expect(TokenKind::ColonEq);
                let value = Box::new(self.expr());
                self.expect(TokenKind::KwIn);
                let body = Box::new(self.expr());
                let span = span.merge(body.span);
                return (ExprKind::Let(Box::new(Let { var, value, body })), span).into();
            }
            Ok(tok) => {
                self.report((ParseErrorKind::new_expected_expr(tok.to_string()), span).into());
                Expr::new_err(span)
            }
            Err(e) => {
                self.report(e.into());
                Expr::new_err(span)
            }
        };

        self.expr_bp_with_lhs(min_bp, lhs)
    }

    fn expr_bp_with_lhs(&mut self, min_bp: u8, lhs: Expr) -> Expr {
        let mut lhs = lhs;
        loop {
            let Token { kind: peek, span } = &self.tokens.peek();
            let op = match peek {
                // TODO: can not recognize a second RParen in code like `a))`
                Ok(TokenKind::EOF)
                | Ok(TokenKind::RParen)
                | Ok(TokenKind::KwDef)
                | Ok(TokenKind::ColonEq)
                | Ok(TokenKind::KwIn) => break,
                Ok(TokenKind::Plus) => Infix::Add,
                Ok(TokenKind::Minus) => Infix::Sub,
                Ok(TokenKind::Star) => Infix::Mul,
                Ok(TokenKind::Slash) => Infix::Div,
                Ok(TokenKind::Percent) => Infix::Mod,
                Ok(TokenKind::Dot) => Infix::Dot,
                Ok(TokenKind::Colon) => Infix::Colon,
                Ok(TokenKind::DStar) => Infix::Pow,
                Ok(TokenKind::DArrow) => Infix::Lambda,
                Ok(TokenKind::Arrow) => Infix::Imply,
                Ok(TokenKind::Pipe) => Infix::Pipe,
                Ok(TokenKind::Atom(_))
                | Ok(TokenKind::Ident(_))
                | Ok(TokenKind::LParen)
                | Ok(TokenKind::KwAtom)
                | Ok(TokenKind::KwType) => Infix::Apply,
                Ok(_) => {
                    let (tok, span) = self.tokens.next().unwrap_kind();
                    // Comsume this bad token and return
                    self.report(
                        (
                            ParseErrorKind::Unexpected {
                                unexpected: tok.to_string(),
                            },
                            span,
                        )
                            .into(),
                    );
                    continue;
                }
                Err(_) => {
                    let e = self.tokens.next().kind.unwrap_err();
                    self.report(e.into());
                    continue;
                }
            };

            let (l_bp, r_bp) = op.binding_power();
            if l_bp < min_bp {
                break;
            } else if l_bp == min_bp {
                self.report(
                    (
                        ParseErrorKind::NonAssociativeOp {
                            op_str: op.to_string(),
                        },
                        *span,
                    )
                        .into(),
                );
            }

            if op == Infix::Apply {
                let rhs = self.expr_bp(r_bp);
                let span = lhs.span.merge(rhs.span);
                lhs = (
                    ExprKind::Application(Box::new(Application {
                        func: Box::new(lhs),
                        arg: Box::new(rhs),
                    })),
                    span,
                )
                    .into();
                continue;
            }

            let op_span = self.tokens.next().span;
            let rhs = self.expr_bp(r_bp);
            let span = lhs.span.merge(rhs.span);

            lhs = (
                if op == Infix::Colon {
                    ExprKind::Annotated(Box::new(Annotated {
                        expr: Box::new(lhs),
                        type_expr: Box::new(rhs),
                    }))
                } else if op == Infix::Lambda {
                    ExprKind::Lambda(Box::new(Lambda {
                        param: Box::new(lhs),
                        body: Box::new(rhs),
                    }))
                } else {
                    ExprKind::Infix(Box::new(InfixExpr {
                        op,
                        op_span,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }))
                },
                span,
            )
                .into();
        }

        lhs
    }
}
