extern printf;

fn foo(n) {
    printf("foo: %d\n", n);
}

fn main() {
    foo(42);
    foo(1337);
    return 0;
}
