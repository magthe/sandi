with (import (import ./nix/sources.nix).nixpkgs {});

let
  def = import ./default.nix;

  shell-pkgs = with haskellPackages;
    [cabal-install
     ghcid
     ghcide
     hlint
     niv
     ormolu
    ];

in
def.sandi.overrideAttrs (attrs: {
  src = null;
  buildInputs = shell-pkgs ++ attrs.buildInputs;
})
