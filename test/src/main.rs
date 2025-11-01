use pretty_fmt::{PrettyFmt, impl_display_for_pretty_fmt};
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
enum Expr {
    Number(f64),
    #[pretty_fmt("UnitThing")]
    Unit,

    InfixOp {
        // #[skip]
        op: char,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl_display_for_pretty_fmt!(Expr);

fn main() {
    let e = Expr::InfixOp {
        op: '+',
        lhs: Box::new(Expr::InfixOp {
            op: '+',
            lhs: Box::new(Expr::Unit),
            rhs: Box::new(Expr::Number(2.)),
        }),
        rhs: Box::new(Expr::Number(2.)),
    };
    println!("{e}")
}
