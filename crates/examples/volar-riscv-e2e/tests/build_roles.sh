#!/bin/bash
# Compile every role TU (-O0, one cc process per TU) into <workdir>/*.o.
set -u
WORK="$1"
cd "$WORK"
while read -r f; do
  o="$(basename "$f" .c).o"
  if [ ! -f "$o" ]; then
    cc -O0 -w -std=c99 -c "$f" -o "$o" || { echo "cc failed: $f"; exit 1; }
  fi
done < manifest.txt
exit 0
