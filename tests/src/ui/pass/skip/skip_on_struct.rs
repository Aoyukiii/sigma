use pretty_fmt;
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
struct Named {
    #[skip]
    attr1: i32,
    attr2: i32,
}

fn main() {
    let named = Named { attr1: 1, attr2: 2 };
    println!("{named}")
}
