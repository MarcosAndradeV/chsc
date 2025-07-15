module main;
include "stdlib/std.chs";

var name[256] char;
fn main() -> int {
    puts("What is your name?\n");
    var len int = read(0, name, 256);
    if (len <= 0) {
        puts("Could not read name\n");
        exit(1);
    }
    puts("Hello "); puts(name);
    return 0;
}
