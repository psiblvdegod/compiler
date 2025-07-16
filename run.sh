if [ -z "$REPO_ROOT" ]; then
  echo "export REPO_ROOT=..."
  exit 1
fi

cd $REPO_ROOT

if [ "$#" -ne 2 ]; then
    echo "error: 2 arguments expected"
    echo "hint: run.sh <source.lang> <destination.exe>"
    exit 2
fi

compiler="./_build/default/bin/app.exe"

if [ ! -f $compiler ]; then
    dune clean
    dune build

    if [ ! -f $compiler ]; then
        echo "error: compiler not found"
        exit 3
    fi
fi

stdlib="./lib/stdlib.s"

if [ ! -f $stdlib ]; then
    echo "error: stdlib.s not found"
    exit 3
fi

dune exec $compiler "$1" program.s
riscv64-unknown-elf-as program.s -o program.o
riscv64-unknown-elf-as $stdlib -o stdlib.o
riscv64-unknown-elf-ld program.o stdlib.o -o "$2"

rm ./program.s ./program.o ./stdlib.o
