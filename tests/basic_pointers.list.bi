:i count 2
:b shell 62
cargo run -q --bin chsc -- -r --use-c tests/basic_pointers.chs
:i returncode 0
:b stdout 237
Generated assembly: tests/basic_pointers.asm
Assembly successful: tests/basic_pointers.o
Linking successful: tests/basic_pointers
Running executable...
*p(10) == x(10)
Modify x using the pointer
*p(20) == x(20)
Modify x
*p(30) == x(30)


:b stderr 0

:b shell 71
rm tests/basic_pointers tests/basic_pointers.o tests/basic_pointers.asm
:i returncode 0
:b stdout 0

:b stderr 0

