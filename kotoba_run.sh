#!/data/data/com.termux/files/usr/bin/sh
# Kotoba run script
# Usage: sh kotoba_run.sh file.kb

set -e

SRC="kotoba.cpp"
BIN="kotoba"

if [ ! -f "$SRC" ]; then
  echo "[Kotoba] kotoba.cpp が見つかりません"
  exit 1
fi

if [ $# -lt 1 ]; then
  echo "使い方: sh kotoba_run.sh <file.kb | ->"
  exit 1
fi

echo "[Kotoba] build..."
clang++ -std=c++20 -O2 "$SRC" -o "$BIN"

echo "[Kotoba] run: $1"
echo "------------------------"
./"$BIN" "$1"
