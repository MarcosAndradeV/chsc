extern fn printf(ptr, ...) -> int;

fn foo(n int) {
    printf("foo: %d\n", n);
}

fn main() -> int {
    foo(42);
    foo(1337);
    return 0;
}
