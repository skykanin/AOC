{
  description = "Dev environment template for AOC2022";

  inputs = {
    # Unofficial library of utilities for managing Nix Flakes.
    flake-utils.url = "github:numtide/flake-utils";

    # Nix package set
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem
    (with flake-utils.lib.system; [ x86_64-linux x86_64-darwin aarch64-darwin ])
    (system: {
      # A Haskell development environment with provided tooling
      devShells.default =
        let
          # The compiler version to use for development
          compiler-version = "ghc94";
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs) lib;
          hpkgs = pkgs.haskell.packages.${compiler-version};
          # HLS
          # Haskell packages to include
          packages = p: with p; [ megaparsec optics-core pretty-simple ];
          # Haskell and shell tooling
          tools = [
            pkgs.cbqn
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
          name = "dev-shell";

          buildInputs = tools ++ libraries;

          LD_LIBRARY_PATH = libraryPath;
          LIBRARY_PATH = libraryPath;
        };
    });
}
