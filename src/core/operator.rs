#[derive(Debug)]
pub enum OpKind {
    Prefix(Prefix),
    Infix(Infix),
    Postfix(Postfix),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Prefix {
    Plus,
    Neg,
    Not,
}

impl Prefix {
    fn binding_power(&self) -> u8 {
        match self {
            Prefix::Plus => 0,
            Prefix::Neg => 0,
            Prefix::Not => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
    Dot,
}

impl Infix {
    fn binding_power(&self) -> (u8, u8) {
        match &self {
            Infix::Add => (0, 0),
            Infix::Sub => (0, 0),
            Infix::Mul => (0, 0),
            Infix::Div => (0, 0),
            Infix::Dot => (0, 0),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Postfix {
    // Call,
    Index,
}

impl Postfix {
    fn binding_power(&self) -> u8 {
        match self {
            Postfix::Index => 0,
        }
    }
}
