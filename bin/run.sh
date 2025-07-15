cd ~/Desktop/compiler/bin
src="./src.lang"
dest="./program.s"
stdlib="../lib/stdlib.s"
compiler="../_build/default/bin/app.exe"

dune exec $compiler $src $dest
riscv64-unknown-elf-as program.s -o program.o
riscv64-unknown-elf-as $stdlib -o print.o
riscv64-unknown-elf-ld program.o print.o -o program
spike pk program

rm ./program ./program.s ./program.o ./print.o
