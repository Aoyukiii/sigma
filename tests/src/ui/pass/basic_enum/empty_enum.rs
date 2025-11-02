use pretty_fmt;
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
#[allow(unused)]
enum Empty {}

fn main() {
    println!("No outputs")
}
