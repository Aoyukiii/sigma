use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use colored::Colorize;

use crate::core::{
    lexer::{LexError, Lexer, Token, TokenKind},
    operator::{Infix, Prefix},
    utils::{PrettyPrint, Span},
};

#[derive(Debug)]
pub struct Syntax {
    pub kind: SyntaxKind,
    pub span: Span,
}

impl Deref for Syntax {
    type Target = SyntaxKind;
    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl PrettyPrint for Syntax {
    fn pretty_print(
        &self,
        ctx: &mut super::utils::PrettyContext,
        w: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        self.kind.pretty_print(ctx, w)?;
        write!(w, " @ {}", self.span)
    }
}

impl Display for Syntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
    }
}

impl Syntax {
    fn new(kind: SyntaxKind, span: Span) -> Self {
        Self { kind, span }
    }

    fn new_err(span: Span) -> Self {
        Self {
            kind: SyntaxKind::Error,
            span,
        }
    }
}

impl DerefMut for Syntax {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

#[derive(Debug)]
pub enum SyntaxKind {
    Ident(String),
    Atom,
    Type,
    AtomLiteral(String),
    Annotated(Box<Annotated>),
    Lambda(Box<Lambda>),
    PrefixExpr(Box<PrefixExpr>),
    InfixExpr(Box<InfixExpr>),
    Let(Box<Let>),
    Error,
}

impl PrettyPrint for SyntaxKind {
    fn pretty_print(
        &self,
        ctx: &mut super::utils::PrettyContext,
        w: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        match self {
            Self::Ident(it) => write!(w, "{}", it.to_string().magenta()),
            Self::Atom => write!(w, "{}", "Atom".yellow()),
            Self::Type => write!(w, "{}", "Type".yellow()),
            Self::AtomLiteral(it) => write!(w, "{}", it.yellow()),
            Self::Annotated(it) => {
                writeln!(w, "Annotated(")?;
                ctx.write_field_ln(w, "expr", it.expr.as_ref())?;
                ctx.write_field_ln(w, "type", it.type_expr.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Lambda(it) => {
                writeln!(w, "Lambda(")?;
                ctx.write_field_ln(w, "param", it.param.as_ref())?;
                ctx.write_field_ln(w, "body", it.body.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::PrefixExpr(it) => {
                writeln!(w, "\"{}\"(", it.op)?;
                ctx.write_field_ln(w, "rhs", it.rhs.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::InfixExpr(it) => {
                writeln!(w, "\"{}\"(", it.op)?;
                ctx.write_field_ln(w, "lhs", it.lhs.as_ref())?;
                ctx.write_field_ln(w, "rhs", it.rhs.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Let(it) => write!(w, "({} := {} in {})", it.var, it.value, it.body),
            Self::Error => write!(w, "{}", "Error".red()),
        }
    }
}

impl Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken { tok_str: String, span: Span },
    UnclosedLParen { span: Span },
    UnclosedRParen { span: Span },
    ExpectedExpr { span: Span },
    UnexpectedExpr { span: Span },
    LexError(LexError),
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    errs: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: Lexer::new(src),
            errs: Vec::new(),
        }
    }

    pub fn parse(mut self) -> (Syntax, Vec<ParseError>) {
        (self.expr(), self.errs)
    }

    pub fn expr(&mut self) -> Syntax {
        let mut expr = self.expr_bp(0);
        if self.lexer.peek_is(TokenKind::RParen) {
            let span = self.lexer.next().span;
            self.errs.push(ParseError::UnclosedRParen { span });
            expr = self.expr_bp_with_lhs(0, expr);
        }
        expr
    }

    fn expr_bp(&mut self, min_bp: u8) -> Syntax {
        if self.lexer.peek_is(TokenKind::RParen) {
            let span = self.lexer.peek().span;
            self.errs.push(ParseError::ExpectedExpr { span });
            return Syntax::new_err(span);
        }

        let Token { kind: next, span } = self.lexer.next();
        let lhs = match next {
            Ok(TokenKind::Atom(it)) => Syntax::new(SyntaxKind::AtomLiteral(it.to_string()), span),
            Ok(TokenKind::Ident(it)) => Syntax::new(SyntaxKind::Ident(it.to_string()), span),
            Ok(TokenKind::KwAtom) => Syntax::new(SyntaxKind::Atom, span),
            Ok(TokenKind::KwType) => Syntax::new(SyntaxKind::Type, span),
            Ok(TokenKind::Plus) | Ok(TokenKind::Minus) | Ok(TokenKind::Not) => {
                let op = match next.unwrap() {
                    TokenKind::Plus => Prefix::Plus,
                    TokenKind::Minus => Prefix::Neg,
                    TokenKind::Not => Prefix::Not,
                    _ => unreachable!(),
                };
                let r_bp = op.binding_power();
                let rhs = self.expr_bp(r_bp);
                Syntax::new(
                    SyntaxKind::PrefixExpr(Box::new(PrefixExpr {
                        op,
                        rhs: Box::new(rhs),
                    })),
                    span,
                )
            }
            Ok(TokenKind::LParen) => {
                let syntax = self.expr_bp(0);
                if !self.lexer.expect(TokenKind::RParen) {
                    self.errs.push(ParseError::UnclosedLParen { span });
                }
                syntax
            }
            Ok(tok) => {
                self.errs.push(ParseError::UnexpectedToken {
                    tok_str: tok.to_string(),
                    span,
                });
                Syntax::new_err(span)
            }
            Err(e) => {
                self.errs.push(ParseError::LexError(e));
                Syntax::new_err(span)
            }
        };

        self.expr_bp_with_lhs(min_bp, lhs)
    }

    fn expr_bp_with_lhs(&mut self, min_bp: u8, lhs: Syntax) -> Syntax {
        let mut lhs = lhs;
        loop {
            let peek = &self.lexer.peek().kind;
            let op = match peek {
                Ok(TokenKind::EOF) | Ok(TokenKind::RParen) => break,
                Ok(TokenKind::Plus) => Infix::Add,
                Ok(TokenKind::Minus) => Infix::Sub,
                Ok(TokenKind::Star) => Infix::Mul,
                Ok(TokenKind::Slash) => Infix::Div,
                Ok(TokenKind::Dot) => Infix::Dot,
                Ok(TokenKind::Colon) => Infix::Colon,
                Ok(TokenKind::DStar) => Infix::Pow,
                Ok(_) => {
                    let (tok, span) = self.lexer.next().unwrap_kind();
                    // Comsume this bad token and return
                    self.errs.push(ParseError::UnexpectedToken {
                        tok_str: tok.to_string(),
                        span,
                    });
                    continue;
                }
                Err(_) => {
                    let e = self.lexer.next().kind.unwrap_err();
                    self.errs.push(ParseError::LexError(e));
                    continue;
                }
            };

            let (l_bp, r_bp) = op.binding_power();
            if l_bp < min_bp {
                break;
            }
            let Token { kind: _, span } = self.lexer.next();
            let rhs = self.expr_bp(r_bp);

            lhs = Syntax::new(
                if op == Infix::Colon {
                    SyntaxKind::Annotated(Box::new(Annotated {
                        expr: Box::new(lhs),
                        type_expr: Box::new(rhs),
                    }))
                } else {
                    SyntaxKind::InfixExpr(Box::new(InfixExpr {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }))
                },
                span,
            );
        }

        lhs
    }
}

#[derive(Debug)]
pub struct Annotated {
    expr: Box<Syntax>,
    type_expr: Box<Syntax>,
}

#[derive(Debug)]
pub struct Lambda {
    param: Box<Syntax>,
    body: Box<Syntax>,
}

#[derive(Debug)]
pub struct PrefixExpr {
    op: Prefix,
    rhs: Box<Syntax>,
}

#[derive(Debug)]
pub struct InfixExpr {
    op: Infix,
    lhs: Box<Syntax>,
    rhs: Box<Syntax>,
}

#[derive(Debug)]
pub struct Let {
    var: Box<Syntax>,
    value: Box<Syntax>,
    body: Box<Syntax>,
}
