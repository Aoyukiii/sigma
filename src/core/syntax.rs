use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use colored::Colorize;

use crate::core::{
    lexer::{LexError, Lexer, Token, TokenKind},
    operator::Infix,
    utils::Span,
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

impl Display for Syntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.kind, self.span)
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
    InfixExpr(Box<InfixExpr>),
    Let(Box<Let>),
    Error,
}

impl Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(it) => write!(f, "{}", it.to_string().magenta()),
            Self::Atom => write!(f, "{}", "Atom".yellow()),
            Self::Type => write!(f, "{}", "Type".yellow()),
            Self::AtomLiteral(it) => write!(f, "{}", it.yellow()),
            Self::Annotated(it) => write!(f, "({}: {})", it.expr, it.type_expr),
            Self::Lambda(it) => write!(f, "(λ {}. {})", it.param, it.body),
            Self::InfixExpr(it) => write!(f, "\"{}\"({}, {})", it.op, it.lhs, it.rhs),
            Self::Let(it) => write!(f, "({} := {} in {})", it.var, it.value, it.body),
            Self::Error => write!(f, "{}", "Error".red()),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    BadToken { tok_str: String },
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
        (self.parse_bp(0), self.errs)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Syntax {
        let Token { kind: next, span } = self.lexer.next();
        let mut lhs = match next {
            Ok(TokenKind::Atom(it)) => Syntax::new(SyntaxKind::AtomLiteral(it.to_string()), span),
            Ok(TokenKind::Ident(it)) => Syntax::new(SyntaxKind::Ident(it.to_string()), span),
            Ok(TokenKind::KwAtom) => Syntax::new(SyntaxKind::Atom, span),
            Ok(TokenKind::KwType) => Syntax::new(SyntaxKind::Type, span),
            Ok(tok) => {
                self.errs.push(ParseError::BadToken {
                    tok_str: tok.to_string(),
                });
                Syntax::new_err(span)
            }
            Err(_) => {
                let (e, span) = self.lexer.next().unwrap_error();
                self.errs.push(ParseError::LexError(e));
                return Syntax::new_err(span);
            }
        };

        loop {
            let peek = self.lexer.peek();
            let op = match peek {
                Ok(TokenKind::EOF) => break,
                Ok(TokenKind::Plus) => Infix::Add,
                Ok(TokenKind::Minus) => Infix::Sub,
                Ok(TokenKind::Star) => Infix::Mul,
                Ok(TokenKind::Slash) => Infix::Div,
                Ok(TokenKind::Dot) => Infix::Dot,
                Ok(tok) => {
                    self.errs.push(ParseError::BadToken {
                        tok_str: tok.to_string(),
                    });
                    // Comsume this bad token and return
                    return Syntax::new_err(self.lexer.next().span);
                }
                Err(_) => {
                    let (e, span) = self.lexer.next().unwrap_error();
                    self.errs.push(ParseError::LexError(e));
                    return Syntax::new_err(span);
                }
            };

            let (l_bp, r_bp) = op.binding_power();
            if l_bp < min_bp {
                break;
            }
            let Token { kind: _, span } = self.lexer.next();
            let rhs = self.parse_bp(r_bp);

            lhs = Syntax::new(
                SyntaxKind::InfixExpr(Box::new(InfixExpr {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })),
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
