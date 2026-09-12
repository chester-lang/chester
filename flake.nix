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
          default = pkgs.stdenv.mkDerivation {
            pname = "chester-compiler-bootstrapped";
            version = "0.1.0";
            src = ./.;
            buildInputs = [
              pkgs.coq
              pkgs.rocqPackages.stdlib
              pkgs.ocamlPackages.ocaml
              pkgs.ocamlPackages.dune_3
              pkgs.ocamlPackages.findlib
              pkgs.ocamlPackages.alcotest
              pkgs.ocamlPackages.ppx_expect
              pkgs.go
            ];
            buildPhase = ''
              # 0. Build Rocq implementation
              coq_makefile -f _CoqProject -o Makefile
              make -j$NIX_BUILD_CORES
              dune build

              # 1. Compile self-hosted sources using Rocq compiler
              dune exec bin/main.exe -- --go -o stage1.go \
                stdlib/std.chester \
                self-hosted/ast.chester \
                self-hosted/cst.chester \
                self-hosted/lexer.chester \
                self-hosted/parser.chester \
                self-hosted/expander.chester \
                self-hosted/elaborator.chester \
                self-hosted/formatter.chester \
                self-hosted/codegen_go.chester \
                self-hosted/cli.chester

              # 2. Build Stage 1 Go binary
              export GOCACHE=$(mktemp -d)
              go build -o stage1 stage1.go

              # 3. Concatenate self-hosted sources
              cat \
                stdlib/std.chester \
                self-hosted/ast.chester \
                self-hosted/cst.chester \
                self-hosted/lexer.chester \
                self-hosted/parser.chester \
                self-hosted/expander.chester \
                self-hosted/elaborator.chester \
                self-hosted/formatter.chester \
                self-hosted/codegen_go.chester \
                self-hosted/cli.chester > self-hosted-all.chester

              # 4. Use Stage 1 to compile self-hosted sources again (Stage 2)
              cat self-hosted-all.chester | ./stage1 > stage2.go

              # 5. Build Stage 2 Go binary
              go build -o stage2 stage2.go
            '';
            installPhase = ''
              mkdir -p $out/bin
              cp stage2 $out/bin/chester
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
              pkgs.ocamlPackages.alcotest
              pkgs.ocamlPackages.ppx_expect
              pkgs.ocamlPackages.yojson
              pkgs.ocamlPackages.ocamlformat
              pkgs.nixfmt
              pkgs.bun
              pkgs.go
              pkgs.nodejs
            ];
          };
        }
      );

      formatter = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          formatterPath = pkgs.lib.makeBinPath [
            pkgs.nixfmt
            pkgs.ocamlformat
            pkgs.ocamlPackages.dune_3
            pkgs.ocamlPackages.ocaml
            pkgs.ocamlPackages.findlib
            pkgs.ocamlPackages.alcotest
            pkgs.ocamlPackages.ppx_expect
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
