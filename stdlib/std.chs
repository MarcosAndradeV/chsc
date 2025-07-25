
fn memset(p *char, byte char, size int) -> *char {
    var i int = 0;
    while(i < size) {
        *(p + i) = byte;
        i = i + 1;
    }
    return p;
}

fn strlen(s *char) -> int {
    var end *char = s;
    while((*s) != 0) {
        s = s + 1;
    }
    return s - end;
}

fn puts(x *char) {
    syscall(1, 1, x, strlen(x));
}

fn read(fd int, buf *char, count int) -> int {
    return syscall(0, fd, buf, count);
}

fn write(fd int, buf *char, buf_len int) -> int {
    return syscall(1, fd, buf, buf_len);
}

fn exit(code int) {
    syscall(60, code);
}
