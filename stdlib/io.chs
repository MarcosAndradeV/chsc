import "stdlib/string.chs";
import "stdlib/linux.chs";

fn put_string(x *char) {
    write(STDOUT, x, strlen(x));
}

fn put_char(x char) {
    write(STDOUT, &x, 1);
}

fn is_print(c int) -> bool {
	return (c >= 32) && (c <= 126);
}

fn is_digit(c int) -> bool {
	return (c >= *"0") && (c <= *"9");
}
