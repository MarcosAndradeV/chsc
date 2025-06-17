:i count 2
:b shell 45
cargo run -q --bin chsc -- -r tests/hello.chs
:i returncode 0
:b stdout 29
Hello, world
Hello, 42 world

:b stderr 0

:b shell 61
rm tests/hello tests/hello.chsi tests/hello.o tests/hello.asm
:i returncode 0
:b stdout 0

:b stderr 0

