:i count 2
:b shell 54
cargo run -q --bin chsc -- -r --use-c examples/fib.chs
:i returncode 0
:b stdout 158
Generated assembly: examples/fib.asm
Assembly successful: examples/fib.o
Linking successful: examples/fib
Running executable...
0
1
1
2
3
5
8
13
21
34
55
89


:b stderr 0

:b shell 47
rm examples/fib examples/fib.o examples/fib.asm
:i returncode 0
:b stdout 0

:b stderr 0

