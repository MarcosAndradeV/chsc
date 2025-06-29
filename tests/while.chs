extern fn printf(*char, ...) -> int;

fn main() -> int {
    var i int = 0;
    while(i < 10) {
        printf("i: %d\n", i);
        i = i + 1;
    }
    return 0;
}
