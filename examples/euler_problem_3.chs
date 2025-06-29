

extern fn printf(*char, ...) -> int;

fn main() -> int {
    var fac int = 2;
    var n int = 600851475143;
    while(n > 1) {
        if((n % fac) == 0) n = n / fac;
        else fac = fac + 1;
    }
    printf("Answer: %d\n", fac);
    return 0;
}
