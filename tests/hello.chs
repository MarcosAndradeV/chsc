extern fn printf(ptr, ...) -> i32;

fn main() -> i32 {
    printf("Hello, World!\n");
    printf("Hello, %d World!\n", 69);
    return 0;
}
