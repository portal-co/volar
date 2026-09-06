import * as spec from "@portal-solutions/volar-spec-ts";

const requiredExports = ["q_bitpack", "vope_bitpack"];
for (const name of requiredExports) {
  if (typeof spec[name] !== "function") {
    throw new Error(`Expected generated export ${name} to be a function`);
  }
}

console.log(`loaded ${Object.keys(spec).length} generated exports`);
