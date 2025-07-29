import "stdlib/string.chs";

fn put_string(x *char) {
    syscall(1, 1, x, strlen(x));
}

fn is_print(c int) -> bool {
	return c >= 32 && c <= 126;
}
