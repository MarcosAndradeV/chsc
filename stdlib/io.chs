import "stdlib/string.chs";
import "stdlib/linux.chs";

fn put_string(x *char) {
    write(STDOUT, x, strlen(x));
}

fn is_print(c int) -> bool {
	return (c >= 32) && (c <= 126);
}
