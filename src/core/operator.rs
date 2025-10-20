use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Prefix {
    Plus,
    Neg,
    Not,
}

impl Prefix {
    pub fn binding_power(&self) -> u8 {
        match self {
            Prefix::Plus => 0,
            Prefix::Neg => 0,
            Prefix::Not => 0,
        }
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
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
    pub fn binding_power(&self) -> (u8, u8) {
        match &self {
            Infix::Add | Infix::Sub => (1, 2),
            Infix::Mul | Infix::Div => (3, 4),
            Infix::Dot => (6, 5),
        }
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Dot => write!(f, "."),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Postfix {
    // Call,
    Index,
}

impl Postfix {
    pub fn binding_power(&self) -> u8 {
        match self {
            Postfix::Index => 0,
        }
    }
}
