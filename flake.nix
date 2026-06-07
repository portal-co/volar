{
  description = "Volar";

  inputs = {
    # Remark: when adding inputs here, don't forget to also add them in the
    # arguments to `outputs` below!
    aeneas.url = "github:aeneasverif/aeneas";
    charon.follows = "aeneas/charon";
    flake-utils.follows = "aeneas/flake-utils";
    nixpkgs.follows = "aeneas/nixpkgs";
    fstar.follows = "aeneas/fstar";
  };

# Remark: keep the list of outputs in sync with the list of inputs above
  # (see above remark)
  outputs = inputs @ { self, flake-utils, nixpkgs, fstar, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            
          ];
        };

        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_2;
        ocamlPackagesStatic = pkgs.pkgsStatic.ocaml-ng.ocamlPackages_5_2;
        coqPackages = pkgs.coqPackages_8_18;
        charon = inputs.charon.packages.${system}.charon;
        charon-portable = inputs.charon.packages.${system}.charon-portable;
        charon-ml = inputs.charon.packages.${system}.charon-ml.override { inherit ocamlPackages; };
        aeneas = inputs.aeneas.packages.${system}.aeneas;
      in
      {
        packages = {
          inherit aeneas aeneas-static aeneas-release aeneas-static-release;
          inherit charon charon-ml;
        };
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.curl
            pkgs.elan
            ocamlPackages.ocaml
            ocamlPackages.ocamlformat_0_27_0
            ocamlPackages.menhir
            ocamlPackages.odoc
            # ocaml-lsp's version must match the ocaml version used. Pinning
            # this here to save everyone a headache.
            ocamlPackages.ocaml-lsp
            pkgs.jq
            pkgs.rustup
            pkgs.rlwrap
            fstar.packages.${system}.fstar
            coqPackages.coq
          ];

          inputsFrom = [
            self.packages.${system}.aeneas
          ];
        };
        checks = {
        };
      });
}
