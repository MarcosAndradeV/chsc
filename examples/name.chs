
extern malloc;
extern memset;
extern printf;
extern getchar;

fn read_str(buf, len) {
    var i = 0;
    while(i < len) {
        var c = getchar();
        *(buf + i) = c;
        if(c == 10) return i;
        i = i + 1;
    }
    return len;
}

fn main() {
    var n = 256;
    var name = malloc(n);
    memset(name, 0, n);
    printf("What is your name?\n");
    n = read_str(name, n);
    printf("Hello, %s\n", name);
    return 0;
}
