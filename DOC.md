# Core Components

## VOLEs

They satisfy the following:

```
u * D + v = q
```

Verifier sends D, expects q. q is precomputed through other means.

For some fields implemented, `+` is actually xor and `*` and. Thus bit rotation is a thing



For public salt `p`,

```
u * D + v + p = q + p
```
allows creating derivatives which only share `u`

Similarly,

```
(u + p) * D + v = q + p * D
```
so we can also derive based on `u`, sharing `v`

Also,
```
p * D + 0
```
represents `p` as a shared constant, usable for external APIs not based on Volar

VOLEs can be added:

```
u1 * D + v1 = q1
u2 * D + v2 = q2

(u1 + u2) * D + (v1 + v2) = (q1 + q2)
```
