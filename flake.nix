{
  description = "Scala multiplatform project with Scala.js and Scala Native";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # sbt launcher (will auto-download 2.0.0-RC7 from project/build.properties)
        sbt = pkgs.sbt.override {
          jre = pkgs.jdk21;
        };

      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            # Java/JVM
            jdk21
            
            # Scala build tool
            sbt
            
            # Scala.js requirements
            nodejs_20
            
            # Scala Native requirements
            clang
            llvm
            zlib
            
            # Additional utilities
            git
          ];

          shellHook = ''
            echo "Scala multiplatform development environment"
            echo "Java version: $(java -version 2>&1 | head -n 1)"
            echo "Node version: $(node --version)"
            echo "clang: $(which clang)"
            echo ""
            echo "Available platforms:"
            echo "  - JVM (default)"
            echo "  - Scala.js (JavaScript)"
            echo "  - Scala Native"
            echo ""
            echo "Use 'sbt -v compile' for verbose output"
            echo "Use 'sbt -debug' for debug output"
          '';
        };
      }
    );
}
