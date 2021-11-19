{
  description = "TIPE";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      ocamlPackages = pkgs.ocamlPackages;
    in {
      # packages.${system}.tipe = ;
      # defaultPackage = self.packages.${system}.tipe;
      devShell = pkgs.mkShell {
        buildInputs = (with ocamlPackages; [
          pkgs.dune_2
          ocaml
          merlin
          ocp-indent
          ocaml-lsp
          menhir
          ppx_inline_test
        ]);
      };
    });
}
