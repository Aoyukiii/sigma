use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
struct Unnamed(f64, f64);

fn main() {
    let unnamed = Unnamed(1.1, 2.2);
    println!("{unnamed}")
}
