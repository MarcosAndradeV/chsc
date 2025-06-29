
extern fn malloc(int) -> ptr;
extern fn memset(ptr, int, int) -> ptr;
extern fn printf(ptr, ...) -> int;
extern fn getchar() -> int;

fn read_str(buf ptr, len int) -> int {
    var i int = 0;
    while(i < len) {
        var c int = getchar();
        *(buf + i) = c;
        if(c == 10) return i;
        i = i + 1;
    }
    return len;
}

fn main() -> int {
    var n int = 256;
    var name ptr = malloc(n);
    memset(name, 0, n);
    printf("What is your name?\n");
    n = read_str(name, n);
    printf("Hello, %s\n", name);
    return 0;
}
