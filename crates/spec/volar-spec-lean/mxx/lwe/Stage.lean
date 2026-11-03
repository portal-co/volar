import MxxIR
import MxxRuntime

set_option maxRecDepth 16384
set_option maxHeartbeats 2000000

namespace Stage_mxx_lwe

structure Params where
  unit : Unit

set_option genInjectivity false in
set_option genSizeOf false in
structure generatedRoot.Witness where

abbrev generatedRoot.constraints_0 (w_4_0 : Mxx.Primitives.ExactMatrix 257 1 4 1) (outputs : Mxx.Primitives.ExactMatrix 257 1 4 1) : Prop :=
  outputs = w_4_0

abbrev generatedRoot.body (params : Params) (inputs : Mxx.Primitives.ExactMatrix 257 1 4 3 × Mxx.Primitives.ExactMatrix 257 1 3 1 × Mxx.Primitives.ExactMatrix 257 1 4 1 × Unit) (outputs : Mxx.Primitives.ExactMatrix 257 1 4 1) (_witness : generatedRoot.Witness) : Prop :=
  let _params := params
  let w_0_0 : Mxx.Primitives.ExactMatrix 257 1 4 3 := inputs.1
  let w_1_0 : Mxx.Primitives.ExactMatrix 257 1 3 1 := inputs.2.1
  let w_3_0 : Mxx.Primitives.ExactMatrix 257 1 4 1 := inputs.2.2.1
  let w_2_0 : Mxx.Primitives.ExactMatrix 257 1 4 1 := MxxRuntime.matrixMul w_0_0 w_1_0
  let w_4_0 : Mxx.Primitives.ExactMatrix 257 1 4 1 := MxxRuntime.matrixAdd w_2_0 w_3_0
  generatedRoot.constraints_0 w_4_0 outputs

def generatedRoot (params : Params) (inputs : Mxx.Primitives.ExactMatrix 257 1 4 3 × Mxx.Primitives.ExactMatrix 257 1 3 1 × Mxx.Primitives.ExactMatrix 257 1 4 1 × Unit) (outputs : Mxx.Primitives.ExactMatrix 257 1 4 1) : Prop :=
  ∃ witness : generatedRoot.Witness,
    generatedRoot.body params inputs outputs witness


end Stage_mxx_lwe
