{
  description = "TIPE";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ocamlPackages = pkgs.ocamlPackages;
      commonBuildDeps = with ocamlPackages; [
          pkgs.dune_2
          ocaml
          menhir
      ];
      derivation = { debug ? true } : ocamlPackages.buildDunePackage {
        pname = if debug then "tipe_debug" else "tipe";
        version = "0.1.0";
        useDune2 = true;
        src = ./.;
        nativeBuildInputs = commonBuildDeps;
      };
    in rec {
      packages = {
        tipe = derivation { debug = false; };
        tipe_debug = derivation {};
      };
      defaultPackage = packages.tipe_debug;
      devShell = pkgs.mkShell {
        buildInputs = (with ocamlPackages; [
          merlin
          ocp-indent
          ocaml-lsp
          findlib
        ] ++ commonBuildDeps);
      };
    });
}
