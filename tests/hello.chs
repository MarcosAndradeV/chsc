extern fn printf(*char, ...) -> int;

fn main() -> int {
    printf("Hello, world\n");
    printf("Hello, %d world\n", 42);
    return 0;
}
