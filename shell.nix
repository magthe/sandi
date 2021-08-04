{ sources ? import ./nix/sources.nix, pkgs ? (import sources.nixpkgs { }), }:

with pkgs;

(import ./default.nix { }).shellFor {
  packages = p: [ p.sandi p.omnicodec ];
  withHoogle = true;
  buildInputs = with haskellPackages; [
    niv
    cabal-fmt
    cabal-install
    ghcid
    haskell-language-server
    hlint
    ormolu
  ];

  CABAL_DIR = toString ./. + "/.cabal";
}
