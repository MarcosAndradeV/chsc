:i count 2
:b shell 45
cargo run -q --bin chsc -- -r tests/while.chs
:i returncode 0
:b stdout 50
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

:b shell 61
rm tests/while tests/while.chsi tests/while.o tests/while.asm
:i returncode 0
:b stdout 0

:b stderr 0

