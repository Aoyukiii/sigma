use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
enum EnumVariants {
    Empty,
    Named { attr1: i32, attr2: i32 },
    Unnamed(i32, i32),
}

fn main() {
    let empty = EnumVariants::Empty;
    let named = EnumVariants::Named { attr1: 1, attr2: 2 };
    let unnamed = EnumVariants::Unnamed(3, 4);
    println!("{empty}");
    println!("{named}");
    println!("{unnamed}");
}
