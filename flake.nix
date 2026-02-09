{
  description = "Scala multiplatform project with Scala.js and Scala Native";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    typelevel-nix.url = "github:typelevel/typelevel-nix";
    typelevel-nix.inputs.nixpkgs.follows = "nixpkgs";
    typelevel-nix.inputs.flake-utils.follows = "flake-utils";
    sbt-derivation.url = "github:zaninime/sbt-derivation";
    sbt-derivation.inputs.nixpkgs.follows = "nixpkgs";
    sbt-derivation.inputs.flake-utils.follows = "flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      typelevel-nix,
      sbt-derivation,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ typelevel-nix.overlays.default ];
        };
        src = pkgs.lib.cleanSource ./.;

        sbt = pkgs.sbt.override { jre = pkgs.jdk21; };

        cliPkg = sbt-derivation.lib.mkSbtDerivation {
          inherit pkgs src;
          overrides = { inherit sbt; };
          pname = "chester";
          version = "0.1.0";
          depsSha256 = "sha256-x/v2qK5TcCI0Xa8ofVMB7ZCyytJvx72PwUqwzeRVdnE=";
          nativeBuildInputs = [ pkgs.nodejs_20 ];
          depsWarmupCommand = ''
            export SBT_OPTS="-Dchester.intellijPlugin.enabled=false ${"$"}{SBT_OPTS:-}"
            sbt --batch ";reload plugins; update; reload return; project cliJVM; update; assembly"
          '';
          buildPhase = ''
            export SBT_OPTS="-Dchester.intellijPlugin.enabled=false ${"$"}{SBT_OPTS:-}"
            sbt --batch cliJVM/assembly
          '';
          installPhase = ''
            mkdir -p $out/share/java $out/bin
            jar_path=$(find modules/cli/jvm/target -name "chester-cli-assembly*.jar" | head -n 1)
            if [ -z "$jar_path" ]; then
              echo "assembly jar not found under modules/cli/jvm/target" >&2
              exit 1
            fi
            cp "$jar_path" $out/share/java/chester-cli.jar
            cat > $out/bin/chester <<EOF
            #!${pkgs.runtimeShell}
            exec ${pkgs.jdk21}/bin/java -jar $out/share/java/chester-cli.jar "\$@"
            EOF
            chmod +x $out/bin/chester
          '';
        };

      in
      {
        packages = {
          cli = cliPkg;
          default = cliPkg;
        };

        apps.default = {
          type = "app";
          program = "${cliPkg}/bin/chester";
        };

        devShells.default = pkgs.devshell.mkShell {
          name = "chester-shell";
          imports = [ typelevel-nix.typelevelShell ];
          typelevelShell = {
            jdk.package = pkgs.jdk21;
            nodejs.enable = true;
            nodejs.package = pkgs.nodejs_20;
            native.enable = true;
            native.libraries = [ pkgs.zlib ];
          };
          packages = [ pkgs.git ];
        };
      }
    );
}
