{
  description = "Chester CST Rocq Environment";

  inputs = {
    nixpkgs.url = "https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    {
      self,
      nixpkgs,
      systems,
    }:
    let
      forAllSystems = nixpkgs.lib.genAttrs (import systems);
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.rocqPackages.mkRocqDerivation {
            pname = "chester-cst";
            version = "0.1.0";
            src = ./.;
            buildInputs = [
              pkgs.coq
              pkgs.rocqPackages.stdlib
            ];
            configurePhase = ''
              coq_makefile -f _CoqProject -o Makefile
            '';
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.coq
              pkgs.rocqPackages.stdlib
              pkgs.ocamlPackages.ocaml
              pkgs.ocamlPackages.dune_3
              pkgs.ocamlPackages.findlib
              pkgs.ocamlPackages.alcotest
              pkgs.ocamlPackages.ppx_expect
              pkgs.ocamlPackages.ocamlformat
              pkgs.nixfmt-rfc-style
              pkgs.bun
            ];
          };
        }
      );

      formatter = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          formatterPath = pkgs.lib.makeBinPath [
            pkgs.nixfmt-rfc-style
            pkgs.ocamlformat
            pkgs.ocamlPackages.dune_3
            pkgs.ocamlPackages.ocaml
            pkgs.ocamlPackages.findlib
          ];
        in
        pkgs.writeShellScriptBin "formatter" ''
          export PATH=${formatterPath}:$PATH
          nixfmt flake.nix
          ocamlformat -i $(find bin test -name "*.ml" -o -name "*.mli")
          dune exec bin/chester_fmt.exe -- $(find self-hosted stdlib tests -name "*.chester")
        ''
      );
    };
}
