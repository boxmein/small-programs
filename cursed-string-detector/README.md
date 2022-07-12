# cursed string detector

ptrace()s a process, detects if it has a string in memory, and then slowly
steps through it logging every time a string is in memory

## Requirements

- C compiler
- Linux OS (Kernel version above 3)
- Meson build system (to be fair `gcc -o cursed-string-detector ./cursed-string-detector.c will work`)

## Building

```
meson build
cd build
ninja
./cursed-string-detector
```
## Caveats

- May crash the process under test for no reason