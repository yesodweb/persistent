{
  description =
    "Persistence interface for Haskell allowing multiple storage methods.";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        overlays = [
          haskellNix.overlay
          (final: prev: {
            project = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc966";
              projectFileName = "cabal.project";
              shell = {
                tools = {
                  cabal = "latest";
                  cabal-install = "latest";
                  fourmolu = "latest";
                  ghcid = "latest";
                  haskell-language-server = "latest";
                };
                buildInputs = with pkgs; [ postgresql ];
              };
            };
          })
        ];
        flake = pkgs.project.flake { };
      in flake);

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://cache.iog.io" # use GHC binary cache.
    ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
