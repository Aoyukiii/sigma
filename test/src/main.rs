use pretty_fmt::{PrettyFmt, impl_display_for_pretty_fmt};
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
enum Expr {
    Number(f64),
    #[pretty_fmt("UnitThing")]
    Unit,

    InfixOp(#[skip] char, Box<Expr>, Box<Expr>),
}

impl_display_for_pretty_fmt!(Expr);

fn main() {
    let e = Expr::InfixOp(
        '+',
        Box::new(Expr::InfixOp(
            '+',
            Box::new(Expr::Unit),
            Box::new(Expr::Number(2.)),
        )),
        Box::new(Expr::Number(2.)),
    );
    println!("{e}")
}
