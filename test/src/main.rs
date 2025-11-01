use colored::Colorize;
use pretty_fmt::{PrettyFmt, impl_display_for_pretty_fmt};
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
enum Expr {
    #[pretty_fmt("{}", arg0)]
    Number(f64),
    #[header("U")]
    Unit,

    #[header("{} ", op.to_string().yellow().bold())]
    InfixOp {
        #[skip]
        op: char,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    #[pretty_fmt("{}", arg0.with_ctx(ctx))]
    Big(Box<Big>),
}

impl_display_for_pretty_fmt!(Expr, Big);

#[derive(PrettyFmt)]
#[header("BIGGGG")]
struct Big {
    a: i32,
    b: i32,
    #[skip]
    _meta: (),
}

fn main() {
    let e = Expr::InfixOp {
        op: '+',
        lhs: Box::new(Expr::InfixOp {
            op: '+',
            lhs: Box::new(Expr::Unit),
            rhs: Box::new(Expr::Big(Box::new(Big {
                a: 1,
                b: 2,
                _meta: (),
            }))),
        }),
        rhs: Box::new(Expr::Number(2.)),
    };
    println!("{e}")
}
