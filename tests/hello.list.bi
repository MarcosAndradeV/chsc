:i count 2
:b shell 53
cargo run -q --bin chsc -- -r --use-c tests/hello.chs
:i returncode 0
:b stdout 155
Generated assembly: tests/hello.asm
Assembly successful: tests/hello.o
Linking successful: tests/hello
Running executable...
Hello, world
Hello, 42 world


:b stderr 0

:b shell 44
rm tests/hello tests/hello.o tests/hello.asm
:i returncode 0
:b stdout 0

:b stderr 0

