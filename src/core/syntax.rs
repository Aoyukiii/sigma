use std::ops::{Deref, DerefMut};

use crate::core::{
    lexer::{Lexer, Token, TokenKind},
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
    AtomLiteral(String),
    Annotated(Box<Annotated>),
    Lambda(Box<Lambda>),
    InfixExpr(Box<InfixExpr>),
    Let(Box<Let>),
    Error,
}

#[derive(Debug)]
pub enum ParseError {
    ExpectExpr,
    BadToken { tok_str: String },
}

struct Parser<'a> {
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

    fn parse(&mut self) -> Syntax {
        self.parse_bp(0)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Syntax {
        let Token { kind: next, span } = self.lexer.next();
        let mut lhs = match next {
            TokenKind::Atom(it) => Syntax::new(SyntaxKind::AtomLiteral(it.to_string()), span),
            t => {
                self.errs.push(ParseError::BadToken {
                    tok_str: format!("{}", t),
                });
                Syntax::new_err(span)
            }
        };

        loop {
            let peek = self.lexer.peek();
            let op = match peek {
                TokenKind::EOF => break,
                TokenKind::Plus => Infix::Add,
                TokenKind::Minus => Infix::Sub,
                // More branches here...
                t => {
                    self.errs.push(ParseError::BadToken {
                        tok_str: format!("{}", t),
                    });
                    // Comsume this bad token and return
                    return Syntax::new_err(self.lexer.next().span);
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
