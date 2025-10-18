use std::ops::{Deref, DerefMut};

use crate::core::{
    lexer::{Lexer, Token, TokenKind, TokenStream},
    operator::{Infix, OpKind},
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
}

#[derive(Debug)]
pub enum ParseError {
    ExpectExpr,
    BadToken { tok: String },
}

fn parse<'a>(tok_stream: &'a mut TokenStream<'a>) -> Result<Syntax, Vec<ParseError>> {
    parse_bp(tok_stream, 0)
}

fn parse_bp<'a>(
    tok_stream: &'a mut TokenStream<'a>,
    min_bp: u8,
) -> Result<Syntax, Vec<ParseError>> {
    if let Some(tok) = tok_stream.next() {
        let Token { kind, span } = tok;
        let lhs = match kind {
            TokenKind::Atom(atom) => Syntax::new(SyntaxKind::AtomLiteral(atom.to_string()), span),
            tok => {
                return Err(vec![ParseError::BadToken {
                    tok: format!("{}", tok),
                }]);
            }
        };

        loop {
            // [TODO] Do we need to distinguish all 3 kinds in the AST?
            if let Some(Token { kind, span }) = tok_stream.peek() {
                let op = match kind {
                    TokenKind::Plus => Infix::Add,
                    TokenKind::Minus => Infix::Sub,
                    _ => todo!(),
                };
                let (l_bp, r_bp) = op.binding_power();
                if l_bp < min_bp {
                    break;
                }
                {
                    tok_stream.next();
                }
                let rhs = parse_bp(tok_stream, r_bp)?;

                lhs = Syntax::new(
                    SyntaxKind::InfixExpr(Box::new(InfixExpr {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })),
                    span.clone(),
                );
            } else {
                break;
            }
        }

        todo!()
    } else {
        Err(vec![ParseError::ExpectExpr])
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
