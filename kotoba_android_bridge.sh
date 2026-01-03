#!/data/data/com.termux/files/usr/bin/sh
# kotoba_android_bridge.sh
# pseudo-Android loop: shows JSON and lets you simulate button clicks

set -e
BIN="./kotoba_runtime"
APP="app.kb"

if [ ! -x "$BIN" ]; then
  echo "[bridge] build kotoba_runtime..."
  clang++ -std=c++20 -O2 kotoba_runtime.cpp -o kotoba_runtime
fi

echo "[bridge] start (type text, or /btn payload, or /q)"
"$BIN" "$APP" << EOF
TEXT:起動
EOF

while true; do
  printf "> "
  IFS= read -r line || break
  if [ "$line" = "/q" ]; then break; fi
  if echo "$line" | grep -q "^/btn "; then
    payload="${line#/btn }"
    echo "BTN:$payload" | "$BIN" "$APP"
  else
    echo "TEXT:$line" | "$BIN" "$APP"
  fi
done
