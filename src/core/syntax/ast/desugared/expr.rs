use crate::core::utils::span::Span;

#[derive(Debug)]
pub struct DesugaredExpr {
    pub kind: DesugaredExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum DesugaredExprKind {
    Ident(StringId),
    Atom,
    Type,
    AtomLiteral(String),
    Lambda(Box<Lambda>),
    Application(Box<Application>),
    Let(Box<Let>),
    Error,
}
#[derive(Debug)]
pub struct Lambda {
    pub param: Box<DesugaredExpr>,
    pub body: Box<DesugaredExpr>,
}

#[derive(Debug)]
pub struct Application {
    pub func: Box<DesugaredExpr>,
    pub arg: Box<DesugaredExpr>,
}

#[derive(Debug)]
pub struct Let {
    pub var: Box<DesugaredExpr>,
    pub value: Box<DesugaredExpr>,
    pub body: Box<DesugaredExpr>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct StringId(usize);
