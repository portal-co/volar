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

structure Eval (n : Nat) where
  target : List UInt8
  deriving Inhabited

structure Garble (n : Nat) where
  base : List UInt8
  deriving Inhabited

structure GarbleTable (n : Nat) where
  table : List (List UInt8)
  deriving Inhabited

structure GlobalSecret (n : Nat) where
  secret : List UInt8
  deriving Inhabited

def absorb_args_bases {n : Nat} (d : DigestSt) (args : List ((Garble n))) : DigestSt := Id.run do
  let mut d := d
  match args with
    | [] =>
      return d
    | first :: rest =>
    d := DigestSt.update d (first.base)
    return (absorb_args_bases d rest)

namespace Eval

def zero {n : Nat} : Eval n := 
  { target := (List.replicate n (0 : UInt8)) }

def open_ {n : Nat} (self : Eval n) (garble : (Garble n)) : List UInt8 := 
  (List.ofFn (fun (i : Fin n) => (self.target[i.val]! ^^^ garble.base[i.val]!)))

def to_share {n : Nat} (self : Eval n) : (Eval (n / 8)) := Id.run do
  return { target := (List.ofFn (fun (i : Fin (n / 8)) => (Id.run do
    let mut v := (0 : UInt8)
    for h : j in [(0):(8)] do
      let bit := (self.target[((i.val * (8)) + j)]! &&& ((1) : UInt8))
      v := v ||| (bit <<< (j).toUInt8)

    return v
  ))) }

def and_via_table {n : Nat} (self : Eval n) (other : (Eval n)) (table : (GarbleTable n)) (hash : List UInt8 → List UInt8) : (Eval n) := Id.run do
  let index := ((if ((self.target[(0)]! &&& ((1) : UInt8)) == (1)) then (1) else (0)) ||| (if ((other.target[(0)]! &&& ((1) : UInt8)) == (1)) then (2) else (0)))
  let hash := (Id.run do
    let mut d := DigestSt.new
    d := DigestSt.update d (self.target)
    d := DigestSt.update d (other.target)
    return (DigestSt.finalize d hash)
  )
  return { target := (List.ofFn (fun (i : Fin n) => (hash[i.val]! ^^^ table.table[index]![i.val]!))) }

end Eval

namespace Eval

def bitxor {n : Nat} (self : Eval n) (rhs : (Eval n)) : Eval n := 
  { target := (List.ofFn (fun (i : Fin n) => (self.target[i.val]! ^^^ rhs.target[i.val]!))) }

end Eval

instance {n : Nat} : HXor (Eval n) (Eval n) (Eval n) := ⟨Eval.bitxor⟩

namespace Garble

def zero {n : Nat} : Garble n := 
  { base := (List.replicate n (0 : UInt8)) }

def and_result {n : Nat} (self : Garble n) (b : (Garble n)) (hash : List UInt8 → List UInt8) : Garble n := Id.run do
  let mut d := DigestSt.new
  d := DigestSt.update d (self.base)
  d := DigestSt.update d (b.base)
  let hash := (DigestSt.finalize d hash)
  return { base := (List.ofFn (fun (i : Fin n) => hash[i.val]!)) }

def action_result_base {n : Nat} (self : Garble n) (args : List ((Garble n))) (bit : Nat) (hash : List UInt8 → List UInt8) : Garble n := Id.run do
  let mut d := DigestSt.new
  let domain_separator := ([((172) : UInt8)] : List UInt8)
  d := DigestSt.update d (domain_separator)
  d := DigestSt.update d (self.base)
  let mut d_1 := (absorb_args_bases d args)
  let bit_bytes := (u64LeBytes (UInt64.ofNat bit))
  d_1 := DigestSt.update d_1 (bit_bytes)
  let hash := (DigestSt.finalize d_1 hash)
  return { base := (List.ofFn (fun (i : Fin n) => hash[i.val]!)) }

def share {n : Nat} (self : Garble n) (target : List UInt8) : (Eval n) := 
  { target := (List.ofFn (fun (i : Fin n) => (self.base[i.val]! ^^^ target[i.val]!))) }

def to_share {n : Nat} (self : Garble n) : (Garble (n / 8)) := Id.run do
  return { base := (List.ofFn (fun (i : Fin (n / 8)) => (Id.run do
    let mut v := (0 : UInt8)
    for h : j in [(0):(8)] do
      let bit := (self.base[((i.val * (8)) + j)]! &&& ((1) : UInt8))
      v := v ||| (bit <<< (j).toUInt8)

    return v
  ))) }

end Garble

namespace GlobalSecret

def new {n : Nat} (secret : List UInt8) : GlobalSecret n := Id.run do
  let mut secret := secret
  secret := secret.set (0) (secret[(0)]! ||| (1))
  return { secret := secret }

def secret_m {n : Nat} (self : GlobalSecret n) : List UInt8 := 
  self.secret

def encode {n : Nat} (self : GlobalSecret n) (garble : (Garble n)) (value : Bool) : (Eval n) := 
  { target := (List.ofFn (fun (i : Fin n) => (if value then (self.secret[i.val]! ^^^ garble.base[i.val]!) else garble.base[i.val]!))) }

def gen_and_table_pol {n : Nat} (self : GlobalSecret n) (a : (Garble n)) (b : (Garble n)) (pa : Bool) (pb : Bool) (hash : List UInt8 → List UInt8) : (GarbleTable n) := Id.run do
  let result_base := a.and_result (b) hash
  let mut table := (List.ofFn (fun (_ : Fin n) => (List.replicate n (0 : UInt8))))
  for h : i in [(0):(4)] do
    let av := ((i &&& (1)) != (0))
    let bv := ((i &&& (2)) != (0))
    let ea := self.encode (a) (av)
    let eb := self.encode (b) (bv)
    let row := (((ea.target[(0)]! &&& ((1) : UInt8))).toNat ||| (((eb.target[(0)]! &&& ((1) : UInt8))).toNat <<< (1)))
    let result_label := self.encode (result_base) (((av != pa) && (bv != pb)))
    let mut d := DigestSt.new
    d := DigestSt.update d (ea.target)
    d := DigestSt.update d (eb.target)
    let hash := (DigestSt.finalize d hash)
    table := table.set row ((List.ofFn (fun (j : Fin n) => (hash[j.val]! ^^^ result_label.target[j.val]!))))

  return { table := table }

def one_wire_eval {n : Nat} (self : GlobalSecret n) : (Eval n) := 
  self.encode ((Garble.zero)) (true)

def not_garble {n : Nat} (self : GlobalSecret n) (a : (Garble n)) : (Garble n) := 
  { base := (List.ofFn (fun (i : Fin n) => (a.base[i.val]! ^^^ self.secret[i.val]!))) }

def gen_and_table {n : Nat} (self : GlobalSecret n) (a : (Garble n)) (b : (Garble n)) (hash : List UInt8 → List UInt8) : (GarbleTable n) := 
  self.gen_and_table_pol (a) (b) (false) (false) hash

end GlobalSecret

