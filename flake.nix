{
  description = "Haskell ";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    pre-commit-hooks.url = "github:hercules-ci/pre-commit-hooks.nix/flakeModule";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } ({ lib, ... }: {
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.pre-commit-hooks.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          hlib = pkgs.haskell.lib.compose;
        in
        {
          haskellProjects.hage = {
            name = "hage";
            root = lib.cleanSourceWith {
              src = ./.;
              filter = path: type: !lib.hasSuffix ".nix" (baseNameOf path);
            };
            modifier = lib.flip lib.pipe [
              (hlib.addBuildDepend pkgs.age)
              (hlib.generateOptparseApplicativeCompletion "hage")
            ];
            overrides = final: prev: {
              bech32 = lib.pipe prev.bech32 [
                hlib.unmarkBroken
                # The test suite couldn't find the executable for some reason. Crude fix.
                (hlib.addBuildDepend (hlib.dontCheck (hlib.unmarkBroken prev.bech32)))
              ];
            };
          };
          packages.default = config.packages.hage;

          checks.hage-cli-test = pkgs.runCommand "hage-cli-test"
            {
              nativeBuildInputs = [ config.packages.hage pkgs.age ];
            } ''
            set -x

            # hage to-recipient-key

            age-keygen -o key.txt
            cat key.txt
            age-keygen -y key.txt >expected
            hage to-recipient-key <key.txt >actual
            diff expected actual

            # hage encrypt

            example=${pkgs.bash}/bin/bash
            hage encrypt --recipient=$(cat actual) --output encrypted <$example
            age --decrypt -i key.txt -o decrypted encrypted
            cmp $example decrypted

            set +x
            touch $out
          '';

          devShells.default = config.devShells.hage.overrideAttrs (o: {
            nativeBuildInputs = o.nativeBuildInputs ++ [
              pkgs.nixpkgs-fmt
              pkgs.age
              pkgs.ghcid
            ];
            shellHook = ''
              ${config.pre-commit.installationScript}

              echo "Welcome to the dev shell"
              echo "To run the tests, type"
              echo "  live-tests"
              alias live-tests="ghcid --command 'cabal v2-repl hage:test:tests' --test main"

              # nix develop loses system completions :(
              # XDG_DATA_DIRS="$XDG_DATA_DIRS:/run/current-system/sw/share"
            '';
          });

          devShells.try = pkgs.mkShell {
            shellHook = ''
              echo "This shell has the built hage package in it."
              echo "Here you can see how it behaves when added to a shell."
            '';
            nativeBuildInputs = [
              config.packages.default
            ];
          };

          pre-commit.settings.hooks.nixpkgs-fmt.enable = true;
        };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.
        herculesCI.ciSystems = [
          "x86_64-linux"
          "aarch64-darwin"
        ];
      };
    });
}
