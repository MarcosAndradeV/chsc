extern printf;

fn main() {
    var a = 0;
    var b = 1;
    while(a < 100) {
        printf("%d\n", a);
        var c = b;
        b = a + b;
        a = c;
    }
}
