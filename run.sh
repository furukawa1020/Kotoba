#!/data/data/com.termux/files/usr/bin/bash

cd "$(dirname "$0")" || exit 1
echo "=== Kotoba Runtime ==="
echo "Time: $(date)" > logs/last.log
./kotoba work/current.kb >> logs/last.log 2>&1
cat logs/last.log
