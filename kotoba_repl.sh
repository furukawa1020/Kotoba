#!/data/data/com.termux/files/usr/bin/sh
# Kotoba REPL (shell-based, no changes to kotoba.cpp)
# Usage: sh kotoba_repl.sh
#
# Commands:
#   :q       quit
#   :clear   clear current buffer
#   :show    show current buffer
#   :save X  save buffer to file X (e.g., :save demo.kb)

set -e

BIN="./kotoba"
SRC="kotoba.cpp"
TMP=".kotoba_repl_tmp.kb"

# ensure binary exists (build if missing)
if [ ! -x "$BIN" ]; then
  echo "[Kotoba REPL] kotoba binary not found. building..."
  clang++ -std=c++20 -O2 "$SRC" -o kotoba
fi

# init temp
: > "$TMP"

echo "Kotoba REPL"
echo "  enter Kotoba code one line at a time."
echo "  commands: :q  :clear  :show  :save <file>"
echo "------------------------------------------"

while true; do
  printf "kb> "
  # read one line (EOF = quit)
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
      cat "$TMP"
      echo "------------------"
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

  # append line to buffer
  printf "%s\n" "$line" >> "$TMP"

  # run whole buffer (stateless by design)
  echo "---- run ----"
  set +e
  "$BIN" "$TMP"
  status=$?
  set -e
  echo "------------"

  # if error, keep buffer (so user can inspect/fix by :show / :clear)
  if [ $status -ne 0 ]; then
    echo "[Kotoba REPL] error (buffer kept). use :show or :clear"
  fi
done
