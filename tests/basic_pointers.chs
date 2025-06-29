extern fn printf(ptr, ...) -> int;

fn main() -> int {
    var x int = 10;
    var p ptr = &x;

    printf("*p(%d) == x(%d)\n", *p, x);

    printf("Modify x using the pointer\n");
    *p = 20;

    printf("*p(%d) == x(%d)\n", *p, x);

    printf("Modify x\n");
    x = 30;

    printf("*p(%d) == x(%d)\n", *p, x);

    return 0;
}
