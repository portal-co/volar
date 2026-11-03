-- Auto-generated Lean from volar-spec (volar-compiler printer_lean).
-- Native representation; voof proves this equivalent to the Aeneas
-- extraction of the same Rust source.

/-- Digest accumulation state: the ordered list of absorbed byte
    strings. Models `D: Digest`'s new/update/finalize. -/
structure DigestSt where
  parts : List (List UInt8) := []

def DigestSt.new : DigestSt := {}

def DigestSt.update (d : DigestSt) (x : List UInt8) : DigestSt := ⟨d.parts ++ [x]⟩

def DigestSt.finalize (d : DigestSt) (hash : List UInt8 → List UInt8) : List UInt8 :=
  hash (d.parts.flatten)

/-- Little-endian byte expansion of a `UInt64`. -/
def u64LeBytes (x : UInt64) : List UInt8 :=
  (List.range 8).map (fun i => ((x >>> (8 * i.toUInt64)) &&& 0xff).toUInt8)

def MXX_RING_0 : List Nat := ([(257)] : List Nat)

def poly_add {n : Nat} (a : List Nat) (b : List Nat) (q : Nat) : List Nat := 
  (List.ofFn (fun (i : Fin n) => ((((a[i.val]!) + (b[i.val]!)) % (q)))))

def poly_sub {n : Nat} (a : List Nat) (b : List Nat) (q : Nat) : List Nat := 
  (List.ofFn (fun (i : Fin n) => (((((a[i.val]!) + (q)) - (b[i.val]!)) % (q)))))

def poly_neg {n : Nat} (a : List Nat) (q : Nat) : List Nat := 
  (List.ofFn (fun (i : Fin n) => ((((q) - (a[i.val]!)) % (q)))))

def poly_scale {n : Nat} (a : List Nat) (s : Nat) (q : Nat) : List Nat := 
  (List.ofFn (fun (i : Fin n) => ((((a[i.val]!) * (s)) % (q)))))

def poly_mul {n : Nat} (a : List Nat) (b : List Nat) (q : Nat) : List Nat := 
  (List.ofFn (fun (k : Fin n) => ((((List.range' ((0)) ((n) - ((0)))).foldl (fun acc i => (if (i <= k.val) then ((acc + (((a[i]!) * (b[(k.val - i)]!)) % (q))) % (q)) else (((acc + (q)) - (((a[i]!) * (b[((k.val + n) - i)]!)) % (q))) % (q)))) (((0)))) % (q)))))

def mat_add {r : Nat} {c : Nat} {n : Nat} {d : Nat} (a : List (List (List (List Nat)))) (b : List (List (List (List Nat)))) (moduli : List Nat) : List (List (List (List Nat))) := 
  (List.ofFn (fun (i : Fin r) => (List.ofFn (fun (j : Fin c) => (List.ofFn (fun (t : Fin d) => (poly_add (n := n) ((a[i.val]!)[j.val]!)[t.val]! ((b[i.val]!)[j.val]!)[t.val]! moduli[t.val]!)))))))

def mat_sub {r : Nat} {c : Nat} {n : Nat} {d : Nat} (a : List (List (List (List Nat)))) (b : List (List (List (List Nat)))) (moduli : List Nat) : List (List (List (List Nat))) := 
  (List.ofFn (fun (i : Fin r) => (List.ofFn (fun (j : Fin c) => (List.ofFn (fun (t : Fin d) => (poly_sub (n := n) ((a[i.val]!)[j.val]!)[t.val]! ((b[i.val]!)[j.val]!)[t.val]! moduli[t.val]!)))))))

def mat_neg {r : Nat} {c : Nat} {n : Nat} {d : Nat} (a : List (List (List (List Nat)))) (moduli : List Nat) : List (List (List (List Nat))) := 
  (List.ofFn (fun (i : Fin r) => (List.ofFn (fun (j : Fin c) => (List.ofFn (fun (t : Fin d) => (poly_neg (n := n) ((a[i.val]!)[j.val]!)[t.val]! moduli[t.val]!)))))))

def mat_scale {r : Nat} {c : Nat} {n : Nat} {d : Nat} (a : List (List (List (List Nat)))) (s : List Nat) (moduli : List Nat) : List (List (List (List Nat))) := 
  (List.ofFn (fun (i : Fin r) => (List.ofFn (fun (j : Fin c) => (List.ofFn (fun (t : Fin d) => (poly_scale (n := n) ((a[i.val]!)[j.val]!)[t.val]! s[t.val]! moduli[t.val]!)))))))

def mat_mul {r : Nat} {k : Nat} {c : Nat} {n : Nat} {d : Nat} (a : List (List (List (List Nat)))) (b : List (List (List (List Nat)))) (moduli : List Nat) : List (List (List (List Nat))) := 
  (List.ofFn (fun (i : Fin r) => (List.ofFn (fun (j : Fin c) => (List.ofFn (fun (t : Fin d) => ((List.range' ((0)) ((k) - ((0)))).foldl (fun acc k => (poly_add (n := n) acc (poly_mul (n := n) ((a[i.val]!)[k]!)[t.val]! ((b[k]!)[j.val]!)[t.val]! moduli[t.val]!) moduli[t.val]!)) ((List.replicate (n) (((0))))))))))))

def mat_transpose {r : Nat} {c : Nat} {n : Nat} {d : Nat} (a : List (List (List (List Nat)))) : List (List (List (List Nat))) := 
  (List.ofFn (fun (j : Fin c) => (List.ofFn (fun (i : Fin r) => (a[i.val]!)[j.val]!))))

def mat_identity {r : Nat} {n : Nat} {d : Nat} : List (List (List (List Nat))) := 
  (List.ofFn (fun (i : Fin r) => (List.ofFn (fun (j : Fin r) => (List.ofFn (fun (t : Fin d) => (List.ofFn (fun (k : Fin n) => (if ((i.val == j.val) && (k.val == (0))) then ((1)) else ((0)))))))))))

def mxx_lwe_sample (w_0_0 : List (List (List (List Nat)))) (w_1_0 : List (List (List (List Nat)))) (w_3_0 : List (List (List (List Nat)))) : List (List (List (List Nat))) := 
  let w_2_0 := (mat_mul (r := 4) (k := 3) (c := 1) (n := 1) (d := 1) w_0_0 w_1_0 MXX_RING_0)
  let w_4_0 := (mat_add (r := 4) (c := 1) (n := 1) (d := 1) w_2_0 w_3_0 MXX_RING_0)
  w_4_0

