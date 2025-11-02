#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.pass("src/ui/pass/**/*.rs");
    t.compile_fail("src/ui/fail/**/*.rs");
}
