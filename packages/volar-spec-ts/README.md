# @portal-solutions/volar-spec-ts

The TypeScript generated from the Volar specification. It is published apart
from `@portal-solutions/volar-runtime`, which owns only the small handwritten
primitive and helper layer imported by generated modules.

```ts
import * as spec from "@portal-solutions/volar-spec-ts";
```

Regenerate this package with `cargo run -p xtask -- gen-specs`. The generated
module uses `@ts-nocheck` so it can be executed and packaged while the strict
TypeScript-error count remains an explicit tracked backend-quality metric; run
`npm run typecheck:count --workspace @portal-solutions/volar-spec-ts` to report
that metric.
