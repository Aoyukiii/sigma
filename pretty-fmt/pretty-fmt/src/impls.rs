use std::ops::Deref;

macro_rules! impl_pretty_fmt_for_deref {
    ($($ty:ty),*) => {
        $(
            impl<T: crate::PrettyFmt> crate::PrettyFmt for $ty {
                fn pretty_fmt_with_ctx(&self, ctx: &crate::PrettyContext, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    self.deref().pretty_fmt_with_ctx(ctx, f)
                }
            }
        )*
    };
}

impl_pretty_fmt_for_deref!(
    std::boxed::Box<T>,
    std::rc::Rc<T>,
    std::sync::Arc<T>,
    &T,
    &mut T
);

macro_rules! impl_pretty_fmt_for_display {
    ($($ty:ty),*) => {
        $(
            impl crate::PrettyFmt for $ty {
                fn pretty_fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    std::fmt::Display::fmt(self, f)
                }

                fn pretty_fmt_with_ctx(&self, _: &crate::PrettyContext, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    std::fmt::Display::fmt(self, f)
                }
            }
        )*
    };
}

impl_pretty_fmt_for_display!(
    i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize, f32, f64, bool, char, str,
    String
);

#[macro_export]
macro_rules! impl_display_for_pretty_fmt {
    ($($ty:ty),*) => {
        $(
            impl std::fmt::Display for $ty {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    use pretty_fmt::PrettyFmt;
                    self.pretty_fmt(f)
                }
}
        )*
    };
}
