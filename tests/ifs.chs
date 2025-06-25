extern printf;

fn main() {
    var x;
    x = 7;
    if(x < 10)
        printf("%d < 10\n", x);
    else if(5 < x)
        printf("%d > 5\n", x);
    return 0;
}
