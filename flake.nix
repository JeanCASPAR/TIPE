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
      pname = debug: if debug then "tipe_debug" else "tipe";
      derivation = { debug ? true }: ocamlPackages.buildDunePackage {
        pname = pname debug;
        version = "0.1.0";
        useDune2 = true;
        src = ./.;
        nativeBuildInputs = commonBuildDeps;
      };
      app = { debug ? true }: flake-utils.lib.mkApp rec {
        drv = derivation { inherit debug; };
        exePath = "/bin/${pname debug}.exe";
      };
    in rec {
      packages = {
        tipe = derivation { debug = false; };
        tipe_debug = derivation {};
      };
      defaultPackage = packages.tipe_debug;
      apps = {
        tipe = app { debug = false; };
        tipe_debug = app {};
      };
      defaultApp = apps.tipe_debug;
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
