use pretty_fmt;
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
struct Empty1 {}

#[derive(PrettyFmt)]
#[impl_display]
struct Empty2();

#[derive(PrettyFmt)]
#[impl_display]
struct Empty3;

fn main() {
    let empty1 = Empty1 {};
    let empty2 = Empty2();
    let empty3 = Empty3;
    println!("{empty1}");
    println!("{empty2}");
    println!("{empty3}");
}
