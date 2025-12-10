{
  description = "Scala multiplatform project with Scala.js and Scala Native";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    typelevel-nix.url = "github:typelevel/typelevel-nix";
    typelevel-nix.inputs.nixpkgs.follows = "nixpkgs";
    typelevel-nix.inputs.flake-utils.follows = "flake-utils";
    squish-find-the-brains.url = "github:7mind/squish-find-the-brains";
    squish-find-the-brains.inputs.nixpkgs.follows = "nixpkgs";
    squish-find-the-brains.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, typelevel-nix, squish-find-the-brains }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ typelevel-nix.overlays.default ];
        };
        src = pkgs.lib.cleanSource ./.;

        # sbt launcher (will auto-download 2.0.0-RC7 from project/build.properties)
        sbt = pkgs.sbt.override {
          jre = pkgs.jdk21;
        };

        # Offline, reproducible coursier cache based on generated lockfile
        coursierCache = squish-find-the-brains.lib.mkCoursierCache {
          pkgs = pkgs;
          lockfilePath = ./deps.lock.json;
        };

        # sbt setup script and inputs wired to the cached dependencies
        sbtSetup = squish-find-the-brains.lib.mkSbtSetup {
          pkgs = pkgs;
          coursierCache = coursierCache;
          jdk = pkgs.jdk21;
        };

        cliPkg = pkgs.stdenv.mkDerivation {
          pname = "chester";
          version = "0.1.0";
          src = src;
          nativeBuildInputs = sbtSetup.nativeBuildInputs ++ [ pkgs.nodejs_20 ];
          buildPhase = ''
            export TMPDIR=${"$"}{TMPDIR:-/tmp}
            export HOME=${"$"}TMPDIR/sbt-home
            export COURSIER_CACHE=${"$"}HOME/.cache/coursier
            export SBT_GLOBAL_BASE=${"$"}HOME/.sbt
            export SBT_BOOT_DIRECTORY=${"$"}HOME/.sbt/boot
            export JAVA_HOME=${pkgs.jdk21}
            export JAVA_TOOL_OPTIONS="-Duser.home=${"$"}HOME"
            export SBT_OPTS="-Dsbt.offline=true -Dsbt.boot.directory=${"$"}SBT_BOOT_DIRECTORY -Dsbt.coursier.home=${"$"}COURSIER_CACHE ${"$"}{SBT_OPTS:-}"

            mkdir -p "${"$"}COURSIER_CACHE/cache" "${"$"}SBT_GLOBAL_BASE" "${"$"}HOME/.ivy2"
            cp -r ${coursierCache}/https "${"$"}COURSIER_CACHE/cache/"
            chmod -R u+w "${"$"}COURSIER_CACHE"

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
