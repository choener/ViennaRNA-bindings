with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = {ViennaRNA-bindings = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.ViennaRNA-bindings ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
