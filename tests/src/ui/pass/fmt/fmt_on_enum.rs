use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
enum Header {
    #[pretty_fmt("EMPTY")] // You can only use `pretty_fmt` on empty struct
    Empty,
    #[header("NAMED")]
    Named { attr1: i32, attr2: i32 },
    #[header("UNNAMED")]
    Unnamed(i32, i32),
}

#[derive(PrettyFmt)]
#[impl_display]
enum CustomFmt {
    #[pretty_fmt("EMPTY")]
    Empty,
    #[pretty_fmt("NAMED {} {}", attr1, attr2)]
    Named { attr1: i32, attr2: i32 },
    #[pretty_fmt("UNNAMED {} {}", arg0, arg1)]
    Unnamed(i32, i32),
}

fn main() {
    println!("{}", Header::Empty);
    println!("{}", Header::Named { attr1: 1, attr2: 2 });
    println!("{}", Header::Unnamed(3, 4));
    println!("{}", CustomFmt::Empty);
    println!("{}", CustomFmt::Named { attr1: 1, attr2: 2 });
    println!("{}", CustomFmt::Unnamed(3, 4));
}
