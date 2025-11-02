use pretty_fmt_macros::PrettyFmt;
use pretty_fmt;

#[derive(PrettyFmt)]
#[impl_display]
struct Empty;

fn main() {
    let empty = Empty;
    println!("{empty}")
}
