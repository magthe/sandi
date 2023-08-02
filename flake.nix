{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs"; };

  outputs = { self, nixpkgs }:
    with nixpkgs.legacyPackages.x86_64-linux;
    with builtins;
    let
      hl = haskell.lib;
      hsPkgs = haskell.packages.ghc94;

      extraHsPkgs = hsPkgs.override {
        overrides = self: super: {
          ListLike = hl.dontCheck super.ListLike;
          hlint = super.hlint_3_5;
        };
      };

      hsDevToolFn = p:
        with p;
        if getEnv "NIX_BUILD_SHELL" == "" then [
          cabal-fmt
          haskell-language-server
          hlint
        ] else
          [ ];

      hsPkgsFn = p:
        with p; [
          cmdargs
          conduit
          criterion
          tasty-hunit
          tasty-quickcheck
          tasty-th
        ];
    in {
      devShell.x86_64-linux = mkShell {
        buildInputs = [ (extraHsPkgs.ghcWithPackages hsPkgsFn) cabal-install ]
          ++ hsDevToolFn extraHsPkgs;
      };
    };
}
