:i count 2
:b shell 53
cargo run -q --bin chsc -- -r --use-c tests/while.chs
:i returncode 0
:b stdout 176
Generated assembly: tests/while.asm
Assembly successful: tests/while.o
Linking successful: tests/while
Running executable...
i: 0
i: 1
i: 2
i: 3
i: 4
i: 5
i: 6
i: 7
i: 8
i: 9


:b stderr 0

:b shell 44
rm tests/while tests/while.o tests/while.asm
:i returncode 0
:b stdout 0

:b stderr 0

