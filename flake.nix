{
  description = "Persistence interface for Haskell allowing multiple storage methods.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVer = "ghc902";
      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (oldArgs: {
              overrides =
                prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                  (overlay prev);
            });
          };
        };
      };

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            config.allowBroken = true;
          };

        in
        {
          packages = rec {
            default = persistent;
            inherit (pkgs.haskell.packages.${ghcVer})
              persistent
              persistent-qq
              persistent-template
              persistent-mysql
              persistent-test
              persistent-postgresql
              persistent-sqlite;
          };

          checks = self.packages.${system};

          # for debugging
          # inherit pkgs;

          devShells.default =
            let haskellPackages = pkgs.haskell.packages.${ghcVer};
            in
            haskellPackages.shellFor {
              packages = p: with pkgs.haskell.packages.${ghcVer};
                [
                  persistent
                  persistent-qq
                  persistent-template
                  persistent-redis
                  persistent-mongoDB
                  persistent-mysql
                  persistent-test
                  persistent-postgresql
                  persistent-sqlite
                ];
              withHoogle = true;
              buildInputs = with haskellPackages; [
                haskell-language-server
                fourmolu
                ghcid
                cabal-install
              ] ++ (with pkgs; [
                sqlite
              ]);
              # Change the prompt to show that you are in a devShell
              # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        default = makeHaskellOverlay (prev: hfinal: hprev:
          let
            hlib = prev.haskell.lib;
            # this makeLocal business is rather a hack to deal with keeping the
            # overrides from nixpkgs for these packages for e.g.
            # persistent-sqlite using the nix-provided sqlite.
            makeLocal = n: hlib.overrideSrc hprev.${n} { src = ./. + "/${n}"; };
          in
          {
            persistent = makeLocal "persistent";
            persistent-test = makeLocal "persistent-test";
            persistent-sqlite = makeLocal "persistent-sqlite";
            persistent-postgresql = makeLocal "persistent-postgresql";
            persistent-mysql = makeLocal "persistent-mysql";
            persistent-redis = makeLocal "persistent-redis";
            persistent-mongoDB = makeLocal "persistent-mongoDB";
            persistent-qq = makeLocal "persistent-qq";
            persistent-template = makeLocal "persistent-template";
          });
      };
    };
}
