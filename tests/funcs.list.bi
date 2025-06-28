:i count 2
:b shell 53
cargo run -q --bin chsc -- -r --use-c tests/funcs.chs
:i returncode 0
:b stdout 144
Generated assembly: tests/funcs.asm
Assembly successful: tests/funcs.o
Linking successful: tests/funcs
Running executable...
foo: 42
foo: 1337


:b stderr 0

:b shell 44
rm tests/funcs tests/funcs.o tests/funcs.asm
:i returncode 0
:b stdout 0

:b stderr 0

