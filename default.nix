with (import (import ./nix/sources.nix).nixpkgs {});

let
  t = lib.trivial;
  hl = haskell.lib;

  addBuildTools = (t.flip hl.addBuildTools) [haskellPackages.cabal-install];

  sandiPkg = haskellPackages.developPackage {
    root = ./sandi;
  };

  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: rec {
      sandi = sandiPkg;
    };
  };

  omnicodecPkg = myHaskellPackages.developPackage {
    root = ./omnicodec;
  };

in {
  sandi = sandiPkg;
  omnicodec = omnicodecPkg;
}
