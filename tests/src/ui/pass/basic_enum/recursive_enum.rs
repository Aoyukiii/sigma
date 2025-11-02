use pretty_fmt;
use pretty_fmt_macros::PrettyFmt;

#[derive(PrettyFmt)]
#[impl_display]
enum RecursiveEnum {
    Empty,
    Data(i32),
    Children(Box<RecursiveEnum>, Box<RecursiveEnum>),
}

fn main() {
    let unnamed = RecursiveEnum::Children(
        Box::new(RecursiveEnum::Data(1)),
        Box::new(RecursiveEnum::Children(
            Box::new(RecursiveEnum::Empty),
            Box::new(RecursiveEnum::Children(
                Box::new(RecursiveEnum::Data(3)),
                Box::new(RecursiveEnum::Empty),
            )),
        )),
    );
    println!("{unnamed}")
}
