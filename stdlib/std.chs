module std;

// fn memset(p *char, byte char, size int) -> ptr {
//     var i int = 0;
//     while(i < size) {
//         *(p + i) = byte;
//         i = i + 1;
//     }
//     return p;
// }

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
