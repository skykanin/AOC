{
  description = "Dev environment template for Advent of Code!";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Nix package set
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];

      perSystem = { pkgs, lib, ... }: {
        # A devShell with all kinds of languages I've used for
        # solving Advent of Code throughout the years
        devShells.default =
          let
            # The compiler version to use for development
            compiler-version = "ghc96";
            hpkgs = pkgs.haskell.packages.${compiler-version};
            # HLS
            # Haskell packages to include
            packages = p: with p; [ megaparsec optics-core pretty-simple ];
            # Haskell and shell tooling
            tools = [
              pkgs.cbqn
              pkgs.uiua
              (hpkgs.ghcWithPackages packages)
              hpkgs.ghcid
              hpkgs.fourmolu
              # Always put HLS in the bottom of the list
              # so it doesn't override other exported packages
              # like formatters and ghc...
              hpkgs.haskell-language-server
            ];
            # System libraries that need to be symlinked
            libraries = [ ];
            libraryPath = "${lib.makeLibraryPath libraries}";
          in pkgs.mkShell {
            name = "AOC";

            buildInputs = tools ++ libraries;

            LD_LIBRARY_PATH = libraryPath;
            LIBRARY_PATH = libraryPath;
          };

        formatter = pkgs.alejandra;
    };
  };
}
