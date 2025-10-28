#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ExprId(pub usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DebruijnId(pub usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct StringId(pub usize);
