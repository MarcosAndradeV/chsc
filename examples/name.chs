
extern fn malloc(int) -> ptr;
extern fn memset(ptr, int, int) -> ptr;
extern fn printf(*char, ...) -> int;
extern fn getchar() -> char;

fn read_str(buf *char, len int) -> int {
    var i int = 0;
    while(i < len) {
        var c char = getchar();
        *(buf + i) = c;
        if(c == cast(char)10) return i;
        i = i + 1;
    }
    return len;
}

fn main() -> int {
    var n int = 256;
    var name *char = cast(*char)malloc(n);
    memset(cast(ptr)name, 0, n);
    printf("What is your name?\n");
    n = read_str(name, n);
    printf("Hello, %s\n", name);
    return 0;
}
