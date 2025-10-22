use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use colored::Colorize;

use crate::core::{
    lexer::{LexError, Lexer, Token, TokenKind},
    operator::{Infix, Prefix},
    utils::{PrettyPrint, Span, write_codeblock},
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
    Application(Box<Application>),
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
            Self::Application(it) => {
                writeln!(w, "Applicaion(")?;
                ctx.write_field_ln(w, "func", it.func.as_ref())?;
                ctx.write_field_ln(w, "arg", it.arg.as_ref())?;
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
                writeln!(w, "({}) @ {} (", it.op.to_string().magenta(), it.op_span)?;
                ctx.write_field_ln(w, "rhs", it.rhs.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::InfixExpr(it) => {
                writeln!(w, "({}) @ {} (", it.op.to_string().magenta(), it.op_span)?;
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
pub struct ParseError {
    kind: ParseErrorKind,
    span: Span,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl From<(ParseErrorKind, Span)> for ParseError {
    fn from((kind, span): (ParseErrorKind, Span)) -> Self {
        Self::new(kind, span)
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    UnexpectedToken { tok_str: String },
    BadToken { tok_str: String },
    UnclosedLParen,
    UnclosedRParen,
    ExpectedExpr { tok_str: String },
    UnexpectedExpr { tok_str: String },
    Unknown,
}

impl From<LexError> for ParseError {
    fn from(lex_err: LexError) -> Self {
        match lex_err {
            LexError::BadToken { tok_str, span } => {
                Self::new(ParseErrorKind::BadToken { tok_str }, span)
            }
            LexError::Unknown => Self::new(ParseErrorKind::Unknown, Span::default()),
        }
    }
}

#[derive(Debug)]
pub struct ParseErrorContext<'a> {
    errs: Vec<ParseError>,
    src: &'a str,
}

impl<'a> ParseErrorContext<'a> {
    pub fn new(errs: Vec<ParseError>, src: &'a str) -> Self {
        Self { errs, src }
    }
}

impl<'a> Display for ParseErrorContext<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let errs = &self.errs;
        for err in errs {
            writeln!(
                f,
                "{} {}",
                format!("[repl:{}]", err.span.to_cursors(self.src).0).underline(),
                err.to_string().red()
            )?;
            write_codeblock(f, self.src, err.span)?;
        }
        Ok(())
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParseErrorKind::UnexpectedToken { tok_str } => {
                write!(f, "Unexpected token `{tok_str}`")
            }
            ParseErrorKind::BadToken { tok_str } => {
                write!(f, "Bad token `{tok_str}`")
            }
            ParseErrorKind::UnclosedLParen => {
                write!(f, "Uncolsed `(`")
            }
            ParseErrorKind::UnclosedRParen => {
                write!(f, "Unclosed `)`")
            }
            ParseErrorKind::ExpectedExpr { tok_str } => {
                write!(f, "Expect expression but found `{tok_str}`")
            }
            ParseErrorKind::UnexpectedExpr { tok_str } => {
                write!(f, "Unexpected token `{tok_str}`")
            }
            ParseErrorKind::Unknown => {
                write!(f, "Unknown error")
            }
        }
    }
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
            self.errs
                .push((ParseErrorKind::UnclosedRParen, span).into());
            expr = self.expr_bp_with_lhs(0, expr);
        }
        expr
    }

    fn expr_bp(&mut self, min_bp: u8) -> Syntax {
        if self.lexer.peek_is(TokenKind::RParen) {
            let span = self.lexer.peek().span;
            self.errs.push(
                (
                    ParseErrorKind::ExpectedExpr {
                        tok_str: TokenKind::RParen.to_string(),
                    },
                    span,
                )
                    .into(),
            );
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
                let op_span = span;
                let rhs = self.expr_bp(r_bp);
                let span = op_span.merge(rhs.span);
                Syntax::new(
                    SyntaxKind::PrefixExpr(Box::new(PrefixExpr {
                        op,
                        op_span,
                        rhs: Box::new(rhs),
                    })),
                    span,
                )
            }
            Ok(TokenKind::LParen) => {
                let syntax = self.expr_bp(0);
                if !self.lexer.expect(TokenKind::RParen) {
                    self.errs
                        .push((ParseErrorKind::UnclosedLParen, span).into());
                }
                syntax
            }
            Ok(tok) => {
                self.errs.push(
                    (
                        ParseErrorKind::ExpectedExpr {
                            tok_str: tok.to_string(),
                        },
                        span,
                    )
                        .into(),
                );
                Syntax::new_err(span)
            }
            Err(e) => {
                self.errs.push(e.into());
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
                // TODO: can not recognize a second RParen in code like `a))`
                Ok(TokenKind::EOF) | Ok(TokenKind::RParen) => break,
                Ok(TokenKind::Plus) => Infix::Add,
                Ok(TokenKind::Minus) => Infix::Sub,
                Ok(TokenKind::Star) => Infix::Mul,
                Ok(TokenKind::Slash) => Infix::Div,
                Ok(TokenKind::Dot) => Infix::Dot,
                Ok(TokenKind::Colon) => Infix::Colon,
                Ok(TokenKind::DStar) => Infix::Pow,
                Ok(TokenKind::Atom(_))
                | Ok(TokenKind::Ident(_))
                | Ok(TokenKind::LParen)
                | Ok(TokenKind::KwAtom)
                | Ok(TokenKind::KwType) => Infix::Apply,
                Ok(_) => {
                    let (tok, span) = self.lexer.next().unwrap_kind();
                    // Comsume this bad token and return
                    self.errs.push(
                        (
                            ParseErrorKind::UnexpectedToken {
                                tok_str: tok.to_string(),
                            },
                            span,
                        )
                            .into(),
                    );
                    continue;
                }
                Err(_) => {
                    let e = self.lexer.next().kind.unwrap_err();
                    self.errs.push(e.into());
                    continue;
                }
            };

            let (l_bp, r_bp) = op.binding_power();
            if l_bp < min_bp {
                break;
            }

            if op == Infix::Apply {
                let rhs = self.expr_bp(r_bp);
                let span = lhs.span.merge(rhs.span);
                lhs = Syntax::new(
                    SyntaxKind::Application(Box::new(Application {
                        func: Box::new(lhs),
                        arg: Box::new(rhs),
                    })),
                    span,
                );
                continue;
            }

            let op_span = self.lexer.next().span;
            let rhs = self.expr_bp(r_bp);
            let span = lhs.span.merge(rhs.span);

            lhs = Syntax::new(
                if op == Infix::Colon {
                    SyntaxKind::Annotated(Box::new(Annotated {
                        expr: Box::new(lhs),
                        type_expr: Box::new(rhs),
                    }))
                } else {
                    SyntaxKind::InfixExpr(Box::new(InfixExpr {
                        op,
                        op_span,
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
pub struct Application {
    func: Box<Syntax>,
    arg: Box<Syntax>,
}

#[derive(Debug)]
pub struct PrefixExpr {
    op: Prefix,
    op_span: Span,
    rhs: Box<Syntax>,
}

#[derive(Debug)]
pub struct InfixExpr {
    op: Infix,
    op_span: Span,
    lhs: Box<Syntax>,
    rhs: Box<Syntax>,
}

#[derive(Debug)]
pub struct Let {
    var: Box<Syntax>,
    value: Box<Syntax>,
    body: Box<Syntax>,
}
