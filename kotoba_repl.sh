#!/data/data/com.termux/files/usr/bin/sh
# Kotoba REPL (shell-based)
# Commands:
#   :q       quit
#   :clear   clear buffer
#   :show    show buffer
#   :undo    remove last line
#   :edit    open buffer in editor
#   :save X  save buffer to file X

set -e

BIN="./kotoba"
SRC="kotoba.cpp"
TMP=".kotoba_repl_tmp.kb"
EDITOR_CMD="nano"

# build if needed
if [ ! -x "$BIN" ]; then
  echo "[Kotoba REPL] building kotoba..."
  clang++ -std=c++20 -O2 "$SRC" -o kotoba
fi

# init buffer
: > "$TMP"

echo "Kotoba REPL"
echo "  commands: :q :clear :show :undo :edit :save <file>"
echo "------------------------------------------"

while true; do
  printf "kb> "
  if ! IFS= read -r line; then
    echo ""
    echo "[Kotoba REPL] bye"
    break
  fi

  case "$line" in
    ":q")
      echo "[Kotoba REPL] bye"
      break
      ;;
    ":clear")
      : > "$TMP"
      echo "[Kotoba REPL] cleared"
      continue
      ;;
    ":show")
      echo "----- buffer -----"
      nl -ba "$TMP"
      echo "------------------"
      continue
      ;;
    ":undo")
      if [ -s "$TMP" ]; then
        sed -i '$d' "$TMP"
        echo "[Kotoba REPL] undo last line"
      else
        echo "[Kotoba REPL] buffer empty"
      fi
      continue
      ;;
    ":edit")
      $EDITOR_CMD "$TMP"
      continue
      ;;
    :save\ *)
      out="${line#:save }"
      if [ -z "$out" ]; then
        echo "[Kotoba REPL] file name required"
      else
        cp "$TMP" "$out"
        echo "[Kotoba REPL] saved to $out"
      fi
      continue
      ;;
  esac

  printf "%s\n" "$line" >> "$TMP"

  echo "---- run ----"
  set +e
  "$BIN" "$TMP"
  status=$?
  set -e
  echo "------------"

  if [ $status -ne 0 ]; then
    echo "[Kotoba REPL] error (buffer kept). try :undo or :show"
  fi
done
