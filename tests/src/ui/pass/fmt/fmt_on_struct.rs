use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
#[header("HEADER")]
struct Header {
    attr1: i32,
}

#[derive(PrettyFmt)]
#[impl_display]
#[pretty_fmt("CustomFmt with {} and now doubled to {}", attr1, attr1 * 2)]
struct CustomFmt {
    attr1: i32,
}

fn main() {
    let header = Header { attr1: 1 };
    println!("{header}");
    let custom_fmt = CustomFmt { attr1: 2 };
    println!("{custom_fmt}");
}
