use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
enum EnumVariants {
    Named {
        #[skip]
        attr1: i32,
        attr2: i32,
    },
    Unnamed(#[skip] i32, i32),
}

fn main() {
    let named = EnumVariants::Named { attr1: 1, attr2: 2 };
    let attr1 = match named {
        EnumVariants::Named { attr1, .. } => attr1,
        _ => unreachable!(),
    };
    println!("{named} {attr1}");
    let unnamed = EnumVariants::Unnamed(3, 4);
    let attr1 = match unnamed {
        EnumVariants::Unnamed(attr1, _) => attr1,
        _ => unreachable!(),
    };
    println!("{unnamed} {attr1}");
}
