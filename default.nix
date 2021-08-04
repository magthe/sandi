{ sources ? import ./nix/sources.nix, pkgs ? (import sources.nixpkgs { }), }:

with pkgs;

haskellPackages.extend (haskell.lib.packageSourceOverrides {
  sandi = ./sandi;
  omnicodec = ./omnicodec;
})
