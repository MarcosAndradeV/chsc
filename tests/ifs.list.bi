:i count 2
:b shell 51
cargo run -q --bin chsc -- -r --use-c tests/ifs.chs
:i returncode 0
:b stdout 127
Generated assembly: tests/ifs.asm
Assembly successful: tests/ifs.o
Linking successful: tests/ifs
Running executable...
7 < 10


:b stderr 0

:b shell 38
rm tests/ifs tests/ifs.o tests/ifs.asm
:i returncode 0
:b stdout 0

:b stderr 0

