# Developer ideas

## Conditional compilation

```c
// OS-specific configuration
@if @os(windows) {
    // Windows-specific code
} @else_if @os(macos) {
    // macOS-specific code
} @else_if @os(linux) {
    // Linux-specific code
} @else {
    @exec assert(false, "Unsupported OS");
}

// Backend-specific configuration
@if @backend(C) {
    // C backend logic
} @else_if @backend(fasm) {
    // FASM backend logic
} @else {
    @exec assert(false, "Unsupported backend");
}

// Architecture-specific configuration
@if @arch(x86_64) {
    // 64-bit x86 specific code
} @else_if @arch(aarch64) {
    // ARM64 specific code
} @else {
    @exec assert(false, "Unsupported arch");
}
```
cc -c stdlib/libchs/libchs.c -static -o stdlib/libchs/libchs.a -O3
