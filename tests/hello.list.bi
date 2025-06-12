:i count 3
:b shell 42
cargo run -q --bin chsc -- tests/hello.chs
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 11
tests/hello
:i returncode 0
:b stdout 31
Hello, World!
Hello, 69 World!

:b stderr 0

:b shell 61
rm tests/hello tests/hello.chsi tests/hello.o tests/hello.asm
:i returncode 0
:b stdout 0

:b stderr 0

