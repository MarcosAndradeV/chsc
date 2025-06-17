#include <libc.chs>

fn main() {
    var i;
    i = 0;
    while(i < 10) {
        printf("i: %d\n", i);
        i = i + 1;
    }
    return 0;
}
