with import <nixpkgs> {};
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    BiobaseTypes = ../Lib-BiobaseTypes;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
    ViennaRNA-bindings = ./.;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.ViennaRNA-bindings ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc alex happy c2hs # syb language-c
      BiobaseTypes
      DPutils
      ForestStructures
      OrderedBits
      PrimitiveArray
      SciBaseTypes
    ];
  };
}
