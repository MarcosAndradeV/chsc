:i count 2
:b shell 66
cargo run -q --bin chsc -- -r --use-c examples/euler_problem_3.chs
:i returncode 0
:b stdout 178
Generated assembly: examples/euler_problem_3.asm
Assembly successful: examples/euler_problem_3.o
Linking successful: examples/euler_problem_3
Running executable...
Answer: 6857


:b stderr 0

:b shell 83
rm examples/euler_problem_3 examples/euler_problem_3.o examples/euler_problem_3.asm
:i returncode 0
:b stdout 0

:b stderr 0

