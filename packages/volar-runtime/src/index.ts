// volar-runtime/src/index.ts
// Package entry point — re-exports everything

export type {
  Cloneable,
  FieldElement,
  BlockEncrypt,
  Digest,
  DigestConstructor,
  LengthDoubler,
} from "./interfaces";

export {
  Bit,
  Galois,
  Galois64,
  BitsInBytes,
  BitsInBytes64,
} from "./primitives";

export {
  ilog2,
  wrappingAdd,
  wrappingSub,
  fieldAdd,
  fieldSub,
  fieldMul,
  fieldBitxor,
  fieldBitor,
  fieldBitand,
  fieldShl,
  fieldShr,
  fieldEq,
  fieldNe,
  commit,
  doubleVec,
  asRefU8,
} from "./helpers";
