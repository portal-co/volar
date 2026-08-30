import glob, re

for f in glob.glob('/tmp/ch/f_*.c'):
    if 'piece_' in f:
        continue  # pieces allocate their own correct-sized calloc pools
    src = open(f).read()
    orig = src
    # grow slot arrays
    def grow(m):
        ty, name, n = m.group(1), m.group(2), int(m.group(3))
        if n < 1650:
            return f'{ty} slot_{name}[1650];'
        return m.group(0)
    src = re.sub(r'(Vope|Q|bool|Arr_[A-Za-z0-9_]+) slot_(v\d+)\[(\d+)\];', grow, src)
    # grow callocs
    src = re.sub(r'calloc\((\d+), sizeof\(([^)]+)\)\)',
                 lambda m: f'calloc(1650, sizeof({m.group(2)}))' if int(m.group(1)) < 1650 else m.group(0),
                 src)
    if src != orig:
        open(f, 'w').write(src)
        print('patched', f)
