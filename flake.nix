{
  description = "Chester CST Rocq Environment";

  inputs = {
    nixpkgs.url = "https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.rocqPackages.mkRocqDerivation {
            pname = "chester-cst";
            version = "0.1.0";
            src = ./.;
            buildInputs = [ pkgs.coq pkgs.rocqPackages.stdlib ];
            configurePhase = ''
              coq_makefile -f _CoqProject -o Makefile
            '';
          };
        }
      );

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.coq
              pkgs.rocqPackages.stdlib
            ];
          };
        }
      );
    };
}
