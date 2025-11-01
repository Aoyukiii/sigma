use std::fmt::Display;

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
pub enum Precedence {
    // Lowest precedence here
    TypeAnnotation, // :
    Implication,    // -> =>
    Disjunction,    // || ∨
    Conjunction,    // && ∧
    Equality,       // == !=
    Relational,     // < <= > >=
    Pipe,           // |>
    Shift,          // << >>
    Additive,       // + -
    Multiplicative, // * / %
    Unary,          // - ! ~
    Exponential,    // **
    Projection,     // .
    Application,    // fn appl
} // Highest precedence here

pub enum Associativity {
    Left,
    Right,
    None,
}

impl Precedence {
    const fn binding_power(self, assoc: Associativity) -> (u8, u8) {
        match assoc {
            Associativity::Left => (self as u8 * 2, self as u8 * 2 + 1),
            Associativity::Right => (self as u8 * 2 + 1, self as u8 * 2),
            Associativity::None => (self as u8 * 2, self as u8 * 2),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Prefix {
    Plus,
    Neg,
    Not,
}

impl Prefix {
    #[rustfmt::skip]
    pub const fn binding_power(&self) -> u8 {
        match self {
            Prefix::Plus | Prefix::Neg | Prefix::Not => Precedence::Unary.binding_power(Associativity::None).0,
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
    Mod,
    Dot,
    Colon,
    Pow,
    Apply,
    Lambda,
    Imply,
    Pipe,
}

impl Infix {
    #[rustfmt::skip]
    pub const fn binding_power(&self) -> (u8, u8) {
        match &self {
            Infix::Add | Infix::Sub => Precedence::Additive.binding_power(Associativity::Left),
            Infix::Mul | Infix::Div | Infix::Mod => Precedence::Multiplicative.binding_power(Associativity::Left),
            Infix::Dot => Precedence::Projection.binding_power(Associativity::Left),
            Infix::Colon => Precedence::TypeAnnotation.binding_power(Associativity::None),
            Infix::Pow => Precedence::Exponential.binding_power(Associativity::Right),
            Infix::Apply => Precedence::Application.binding_power(Associativity::Left),
            Infix::Lambda | Infix::Imply => Precedence::Implication.binding_power(Associativity::Right),
            Infix::Pipe => Precedence::Pipe.binding_power(Associativity::Left),
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
            Self::Mod => write!(f, "%"),
            Self::Dot => write!(f, "."),
            Self::Colon => write!(f, ":"),
            Self::Pow => write!(f, "**"),
            Self::Apply => write!(f, "Apply"),
            Self::Lambda => write!(f, "=>"),
            Self::Imply => write!(f, "->"),
            Self::Pipe => write!(f, "|>"),
        }
    }
}
