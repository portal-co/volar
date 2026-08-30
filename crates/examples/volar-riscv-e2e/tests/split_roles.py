#!/usr/bin/env python3
"""Split the generated three-role C module into per-function TUs + a header.

Usage: split_roles.py <roles.c> <workdir>
Writes <workdir>/header.h and one f_NN_<name>.c per function, plus
<workdir>/manifest.txt listing the TUs (header first)."""
import re, os, sys

c_src_path, workdir = sys.argv[1], sys.argv[2]
C_SRC = open(c_src_path).read()
os.makedirs(workdir, exist_ok=True)

def glob_safe(d):
    try:
        return os.listdir(d)
    except FileNotFoundError:
        return []

for f in glob_safe(workdir):
    os.remove(os.path.join(workdir, f))

lines = C_SRC.split('\n')
hdr_end = None
for i, l in enumerate(lines):
    if re.match(r'^(static\s+|extern\s+)?[A-Za-z_][A-Za-z0-9_ \*]*\w\s*\(', l) and l.rstrip().endswith('{'):
        stripped = l.strip()
        if stripped.startswith('static') or stripped.startswith('extern'):
            continue
        hdr_end = i
        break
assert hdr_end is not None, 'no function definition found'
header = '\n'.join(lines[:hdr_end])
rest = '\n'.join(lines[hdr_end:])

extra_externs = re.findall(r'^extern [^;]+;\s*$', rest, flags=re.M)
header += '\n' + '\n'.join(dict.fromkeys(extra_externs)) + '\n'
open(os.path.join(workdir, 'header.h'), 'w').write(header)

chunks = rest.split('\n}\n')
idx = 0
manifest = []
for c in chunks:
    if not c.strip():
        continue
    sig = c.split('(')[0]
    name = re.findall(r'([A-Za-z_][A-Za-z0-9_]*)\s*$', sig.strip())[0]
    fname = os.path.join(workdir, f'f_{idx:02d}_{re.sub(r"[^A-Za-z0-9_]", "_", name)[:60]}.c')
    open(fname, 'w').write('#include "header.h"\n' + c + '\n}\n')
    manifest.append(fname)
    idx += 1

# The wrapper functions' local piece-pool slots are sized by the C emit's
# per-slot count, but the pieces index them by raw global var ids (up to
# the piece's own var-space size) — the same reason the Rust driver sizes
# its pools to TOTAL_VARS. Grow wrapper-local slots/callocs to cover it.
TOTAL = 512
def grow(m):
    ty, name, n = m.group(1), m.group(2), int(m.group(3))
    if n < TOTAL:
        return f'{ty} slot_{name}[{TOTAL}];'
    return m.group(0)
for mf in list(manifest):
    src = open(mf).read()
    if 'piece_' in mf or '_piece_' in src[:400]:
        continue
    patched = re.sub(r'(Vope|Q|bool|Arr_[A-Za-z0-9_]+) slot_(v\d+)\[(\d+)\];', grow, src)
    patched = re.sub(r'calloc\((\d+), sizeof\(([^)]+)\)\)',
                     lambda m: f'calloc({TOTAL}, sizeof({m.group(2)}))' if int(m.group(1)) < TOTAL else m.group(0),
                     patched)
    if patched != src:
        open(mf, 'w').write(patched)

open(os.path.join(workdir, 'manifest.txt'), 'w').write('\n'.join(manifest))
print(f'split: header + {idx} TUs')
