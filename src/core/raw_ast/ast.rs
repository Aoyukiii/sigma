use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use colored::Colorize;

use crate::core::{
    lexer::{LexError, Lexer, Token, TokenKind},
    operator::{Infix, Prefix},
    utils::{PrettyContext, PrettyPrint, Span, write_codeblock},
};

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl From<(ExprKind, Span)> for Expr {
    fn from((kind, span): (ExprKind, Span)) -> Self {
        Self { kind, span }
    }
}

impl Deref for Expr {
    type Target = ExprKind;
    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl PrettyPrint for Expr {
    fn print_ctx(&self, ctx: &mut PrettyContext, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        self.kind.print_ctx(ctx, w)?;
        write!(w, " @ {}", self.span)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
    }
}

impl Expr {
    fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    fn new_err(span: Span) -> Self {
        Expr::new(ExprKind::Error, span)
    }
}

impl DerefMut for Expr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

#[derive(Debug)]
pub struct Stmt {
    kind: StmtKind,
    span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl From<(StmtKind, Span)> for Stmt {
    fn from((kind, span): (StmtKind, Span)) -> Self {
        Self { kind, span }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.kind, self.span)
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Def(Box<Def>),
    Error,
}

impl PrettyPrint for StmtKind {
    fn print_ctx(&self, ctx: &mut PrettyContext, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Self::Def(it) => {
                writeln!(w, "Let(")?;
                ctx.write_field_ln(w, "var", it.var.as_ref())?;
                ctx.write_field_ln(w, "value", it.value.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")?;
            }
            Self::Error => {
                write!(w, "{}", "Error".red())?;
            }
        }
        Ok(())
    }

    fn print(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        write!(w, "{} ", "[Stmt]".yellow())?;
        self.print_ctx(&mut PrettyContext::new(), w)
    }
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
    }
}

#[derive(Debug)]
pub struct Def {
    var: Box<Expr>,
    value: Box<Expr>,
}

#[derive(Debug)]
pub enum ExprKind {
    Ident(String),
    Atom,
    Type,
    AtomLiteral(String),
    Annotated(Box<Annotated>),
    Lambda(Box<Lambda>),
    Application(Box<Application>),
    Prefix(Box<PrefixExpr>),
    Infix(Box<InfixExpr>),
    Error,
}

impl PrettyPrint for ExprKind {
    fn print_ctx(&self, ctx: &mut PrettyContext, w: &mut impl std::fmt::Write) -> std::fmt::Result {
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
            Self::Prefix(it) => {
                writeln!(w, "({}) @ {} (", it.op.to_string().magenta(), it.op_span)?;
                ctx.write_field_ln(w, "rhs", it.rhs.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Infix(it) => {
                writeln!(w, "({}) @ {} (", it.op.to_string().magenta(), it.op_span)?;
                ctx.write_field_ln(w, "lhs", it.lhs.as_ref())?;
                ctx.write_field_ln(w, "rhs", it.rhs.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Error => write!(w, "{}", "Error".red()),
        }
    }
}

impl Display for ExprKind {
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
    Expected { expected: String, found: String },
    BadToken { tok_str: String },
    UnclosedLParen,
    UnclosedRParen,
    ExpectedExpr { tok_str: String },
    NonAssociativeOp { op_str: String },
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
            ParseErrorKind::Expected { expected, found } => {
                write!(f, "Expect `{expected}` but found `{found}`")
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
            ParseErrorKind::NonAssociativeOp { op_str } => {
                write!(f, "Non-associative operator `{op_str}` used continuously")
            }
            ParseErrorKind::Unknown => {
                write!(f, "Unknown error")
            }
        }
    }
}

#[derive(Debug)]
pub enum Ast {
    Stmts(Vec<Stmt>),
    Expr(Expr),
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

    fn expect(&mut self, expected: TokenKind) {
        if !self.lexer.expect(expected.clone()) {
            let peek = self.lexer.next();
            let found = match peek.kind {
                Ok(tok) => tok.to_string(),
                Err(_) => "invalid token".to_string(), // TODO: not a good information
            };
            self.errs.push(
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

    pub fn parse(mut self) -> (Vec<Stmt>, Vec<ParseError>) {
        (self.stmts(), self.errs)
    }

    pub fn repl_parse(mut self) -> (Ast, Vec<ParseError>) {
        (self.stmts_or_expr(), self.errs)
    }

    pub fn stmts_or_expr(&mut self) -> Ast {
        let mut stmts = Vec::new();
        while !self.lexer.is_empty() {
            let peek = &self.lexer.peek().kind;
            let stmt = match peek {
                Ok(TokenKind::KwDef) => {
                    let span = self.lexer.next().span;
                    self.stmt_def(span)
                }
                Ok(_) => return Ast::Expr(self.expr()),
                Err(_) => {
                    let (e, span) = self.lexer.next().unwrap_error();
                    self.errs.push(e.into());
                    (StmtKind::Error, span).into()
                }
            };
            stmts.push(stmt);
        }
        Ast::Stmts(stmts)
    }

    pub fn stmts(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.lexer.is_empty() {
            let peek = &self.lexer.peek().kind;
            let stmt = match peek {
                Ok(TokenKind::KwDef) => {
                    let span = self.lexer.next().span;
                    self.stmt_def(span)
                }
                Ok(_) => break,
                Err(_) => {
                    let (e, span) = self.lexer.next().unwrap_error();
                    self.errs.push(e.into());
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
        if self.lexer.peek_is(TokenKind::RParen) {
            // error recovery for `)`
            let span = self.lexer.next().span;
            self.errs
                .push((ParseErrorKind::UnclosedRParen, span).into());
            return self.expr_bp_with_lhs(0, expr);
        }
        expr
    }

    fn expr_bp(&mut self, min_bp: u8) -> Expr {
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
            return Expr::new_err(span);
        }

        let Token { kind: next, span } = self.lexer.next();
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
                if !self.lexer.expect(TokenKind::RParen) {
                    self.errs
                        .push((ParseErrorKind::UnclosedLParen, span).into());
                }
                syntax
            }
            Ok(TokenKind::RParen) => unreachable!(),
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
                Expr::new_err(span)
            }
            Err(e) => {
                self.errs.push(e.into());
                Expr::new_err(span)
            }
        };

        self.expr_bp_with_lhs(min_bp, lhs)
    }

    fn expr_bp_with_lhs(&mut self, min_bp: u8, lhs: Expr) -> Expr {
        let mut lhs = lhs;
        loop {
            let Token { kind: peek, span } = &self.lexer.peek();
            let op = match peek {
                // TODO: can not recognize a second RParen in code like `a))`
                Ok(TokenKind::EOF)
                | Ok(TokenKind::RParen)
                | Ok(TokenKind::KwDef)
                | Ok(TokenKind::ColonEq) => break,
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
            } else if l_bp == min_bp {
                self.errs.push(
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

            let op_span = self.lexer.next().span;
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

#[derive(Debug)]
pub struct Annotated {
    expr: Box<Expr>,
    type_expr: Box<Expr>,
}

#[derive(Debug)]
pub struct Lambda {
    param: Box<Expr>,
    body: Box<Expr>,
}

#[derive(Debug)]
pub struct Application {
    func: Box<Expr>,
    arg: Box<Expr>,
}

#[derive(Debug)]
pub struct PrefixExpr {
    op: Prefix,
    op_span: Span,
    rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct InfixExpr {
    op: Infix,
    op_span: Span,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Let {
    var: Box<Expr>,
    value: Box<Expr>,
    body: Box<Expr>,
}
