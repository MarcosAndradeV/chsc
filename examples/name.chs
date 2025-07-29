import "stdlib/linux.chs";
import "stdlib/io.chs";

var name[256] char;
fn main() -> int {
    put_string("What is your name?\n");
    var len int = read(0, name, 256);
    if (len <= 0) {
        put_string("Could not read name\n");
        exit(1);
    }
    put_string("Hello "); put_string(name);
    return 0;
}
