if [ -z "$REPO_ROOT" ]; then
  echo "export REPO_ROOT=..."
  exit 1
fi

dir="$REPO_ROOT/test/backend"
cd $dir

for file in *; do
    if [[ "$file" == *.lang ]]; then
        expected_result=$(cat "./$file.expected")

        $REPO_ROOT/run.sh "$dir/$file" "$dir/program"
        actual_result=$(spike pk ./program)

        rm ./program

        if [ "$actual_result" -ne "$expected_result" ]; then
            echo "backend test failed on: $file"
            cat "$file"
            exit 1
        fi
    fi
done

echo "success!"
exit 0
