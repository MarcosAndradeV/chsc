:i count 2
:b shell 43
cargo run -q --bin chsc -- -r tests/ifs.chs
:i returncode 0
:b stdout 7
7 < 10

:b stderr 0

:b shell 53
rm tests/ifs tests/ifs.chsi tests/ifs.o tests/ifs.asm
:i returncode 0
:b stdout 0

:b stderr 0

