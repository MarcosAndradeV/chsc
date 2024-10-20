# CHS (WIP)

A programing language inspired by [Porth](https://gitlab.com/tsoding/porth).

## Goals

- [ ] Compiled to Fasm Assembly
- [ ] Static Type checking
- [ ] Turing Complete
- [ ] Self Hosting

## Code Examples

```
import std.io

fn add(a int, b int) int {
    a + b
}

fn main() {
    if add(10, 20) < 10 {
        writeln("OK")
    } else {
        writeln("Err")
    }
}

```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## References & Inspirations

- BM: [GitHub - tsoding/bm](https://github.com/tsoding/bm)
- Porth: [GitLab - tsoding/porth](https://gitlab.com/tsoding/porth)
- SmallVM: [GitHub - tarekwiz/smallvm](https://github.com/tarekwiz/smallvm)
- IridiumVM: [GitHub - fhaynes/iridium](https://github.com/fhaynes/iridium)
- Inko: [GitHub - inko-lang/inko](https://github.com/inko-lang/inko)
- Boson-lang: [GitHub - Narasimha1997/boson-lang](https://github.com/Narasimha1997/boson-lang)
