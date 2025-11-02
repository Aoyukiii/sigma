// #![deny(unused_variables)]

pub mod ui;

use pretty_fmt;
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
struct Named {
    #[skip]
    attr1: i32,
    attr2: i32,
}

#[derive(PrettyFmt)]
#[impl_display]
struct Unnamed(#[skip] i32, i32);

fn main() {
    let named = Named { attr1: 1, attr2: 2 };
    let attr1 = named.attr1;
    println!("{named} {attr1}");
    let unnamed = Unnamed(3, 4);
    let attr1 = unnamed.0;
    println!("{unnamed} {attr1}");
}
