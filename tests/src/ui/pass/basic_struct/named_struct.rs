
use pretty_fmt;
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
struct Named {
    attr1: i32,
    attr2: f64,
}

fn main() {
    let named = Named {
        attr1: 1,
        attr2: 1.1,
    };
    println!("{named}")
}
