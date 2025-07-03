:i count 2
:b shell 62
cargo run -q --bin chsc -- -r --use-c examples/hello_world.chs
:i returncode 0
:b stdout 182
Generated assembly: examples/hello_world.asm
Assembly successful: examples/hello_world.o
Linking successful: examples/hello_world
Running executable...
Hello, world
Hello, 42 world


:b stderr 0

:b shell 71
rm examples/hello_world examples/hello_world.o examples/hello_world.asm
:i returncode 0
:b stdout 0

:b stderr 0

