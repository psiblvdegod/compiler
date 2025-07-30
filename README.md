# compiler
Project as part of SPbU MM summer school 2025  
Author: Balyshev A.M.  
License: MIT
### Summary
Translates fictional language into `riscv64 Assembly`
### Usage
```bash
dune exec compiler <source> <destination>
```
### Development requirements:
- `dune (>= 3.19)`
- `riscv64-unknown-elf-as (>= 2.43)`
- `riscv64-unknown-elf-ld (>= 2.43)`
- `riscv64` system (native or emulated)
- `ppx_expect (>= 0.17)`
- `ppx_deriving (>= 6.1)`
