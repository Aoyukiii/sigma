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
    InfixOp(Box<InfixOp>),
    Let(Box<Let>),
}

#[derive(Debug)]
pub enum ParseError {
    ExpectExpr,
    BadToken { tok: String },
}

pub fn pratt_parse<'a>(tok_stream: &'a mut TokenStream<'a>) -> Result<Syntax, Vec<ParseError>> {
    if let Some(next) = tok_stream.next() {
        let Token { kind, span } = next;
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
            let op = (match kind {
                TokenKind::Plus => Infix::Add,
                _ => todo!(),
            });
            let power = op.binding_power();
            todo!()
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
pub struct InfixOp {
    op: OpKind,
    lhs: Box<Syntax>,
    rhs: Box<Syntax>,
}

#[derive(Debug)]
pub struct Let {
    var: Box<Syntax>,
    value: Box<Syntax>,
    body: Box<Syntax>,
}
