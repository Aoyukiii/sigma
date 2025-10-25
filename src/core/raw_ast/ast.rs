use std::marker::PhantomData;

use crate::core::{
    operator::{Infix, Prefix},
    raw_ast::{
        diagnostics::{Diagnostics, ParseError, ParseErrorKind},
        expr::{Annotated, Application, Expr, ExprKind, InfixExpr, Lambda, Let, PrefixExpr},
        stmt::{Def, Stmt, StmtKind},
    },
    token::{
        lexer::{Token, TokenKind},
        stream::TokenStream,
    },
    utils::Span,
};

#[derive(Debug)]
pub enum TopLevel {
    Stmts(Vec<Stmt>),
    Expr(Expr),
}

pub struct Parser<'a, T>
where
    T: TokenStream<'a>,
{
    tokens: T,
    errs: Diagnostics,
    _phantom: PhantomData<&'a ()>,
}

impl<'a, T> Parser<'a, T>
where
    T: TokenStream<'a>,
{
    pub fn new(tokens: T) -> Self {
        Self {
            tokens,
            errs: Diagnostics::new(),
            _phantom: PhantomData,
        }
    }

    pub fn with_tokens(&mut self, tokens: T) {
        self.tokens = tokens;
    }

    fn report(&mut self, err: ParseError) {
        self.errs.add_err(err)
    }

    fn expect(&mut self, expected: TokenKind) {
        if !self.tokens.expect(expected.clone()) {
            let peek = self.tokens.next();
            let found = match peek.kind {
                Ok(tok) => tok.to_string(),
                Err(_) => "invalid token".to_string(), // TODO: not a good information
            };
            self.report(
                (
                    ParseErrorKind::Expected {
                        expected: expected.to_string(),
                        found,
                    },
                    peek.span,
                )
                    .into(),
            );
        }
    }

    pub fn parse(&mut self) -> (Vec<Stmt>, Diagnostics) {
        self.errs.clear();
        (self.stmts(), std::mem::take(&mut self.errs))
    }

    pub fn repl_parse(&mut self) -> (TopLevel, Diagnostics) {
        self.errs.clear();
        (self.stmts_or_expr(), std::mem::take(&mut self.errs))
    }

    pub fn stmts_or_expr(&mut self) -> TopLevel {
        let mut stmts = Vec::new();
        while !self.tokens.is_empty() {
            let peek = &self.tokens.peek().kind;
            let stmt = match peek {
                Ok(TokenKind::KwDef) => {
                    let span = self.tokens.next().span;
                    self.stmt_def(span)
                }
                Ok(_) => return TopLevel::Expr(self.expr()),
                Err(_) => {
                    let (e, span) = self.tokens.next().unwrap_error();
                    self.report(e.into());
                    (StmtKind::Error, span).into()
                }
            };
            stmts.push(stmt);
        }
        TopLevel::Stmts(stmts)
    }

    pub fn stmts(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.tokens.is_empty() {
            let peek = &self.tokens.peek().kind;
            let stmt = match peek {
                Ok(TokenKind::KwDef) => {
                    let span = self.tokens.next().span;
                    self.stmt_def(span)
                }
                Ok(_) => break,
                Err(_) => {
                    let (e, span) = self.tokens.next().unwrap_error();
                    self.report(e.into());
                    (StmtKind::Error, span).into()
                }
            };
            stmts.push(stmt);
        }
        stmts
    }

    pub fn stmt_def(&mut self, let_span: Span) -> Stmt {
        let var = Box::new(self.expr());
        self.expect(TokenKind::ColonEq);
        let value = Box::new(self.expr());
        let span = let_span.merge(value.span);
        (StmtKind::Def(Box::new(Def { var, value })), span).into()
    }

    fn expr(&mut self) -> Expr {
        let expr = self.expr_bp(0);
        if self.tokens.peek_is(TokenKind::RParen) {
            // error recovery for `)`
            let span = self.tokens.next().span;
            self.report((ParseErrorKind::UnclosedRParen, span).into());
            return self.expr_bp_with_lhs(0, expr);
        }
        expr
    }

    fn expr_bp(&mut self, min_bp: u8) -> Expr {
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
                Ok(TokenKind::Dot) => Infix::Dot,
                Ok(TokenKind::Colon) => Infix::Colon,
                Ok(TokenKind::DStar) => Infix::Pow,
                Ok(TokenKind::DArrow) => Infix::Lambda,
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
