extern fn printf(ptr, ...) -> int;

fn main() -> int {
    printf("Hello, world\n");
    printf("Hello, %d world\n", 42);
    return 0;
}
