import "stdlib/string.chs";

const STDIN  = 0;
const STDOUT = 1;
const STDERR = 2;

fn put_string(x *char) {
    syscall(1, STDOUT, x, strlen(x));
}

fn is_print(c int) -> bool {
	return (c >= 32) && (c <= 126);
}
