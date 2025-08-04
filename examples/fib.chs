import "stdlib/libchs.chs";

fn main() -> int {
    var a int = 0;
    var b int = 1;
    while(a < 100) {
        putln_int(a);
        var c int = b;
        b = a + b;
        a = c;
    }
    return 0;
}
