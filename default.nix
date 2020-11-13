{ mkDerivation, alex, array, base, BiobaseTypes, bytestring, c2hs
, data-default-class, deepseq, happy, inline-c, parallel
, QuickCheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-silver, tasty-th
}:
mkDerivation {
  pname = "ViennaRNA-bindings";
  version = "0.233.2.1";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring data-default-class inline-c
  ];
  libraryToolDepends = [ alex c2hs happy ];
  testHaskellDepends = [
    array base BiobaseTypes bytestring deepseq parallel QuickCheck
    tasty tasty-hunit tasty-quickcheck tasty-silver tasty-th
  ];
  homepage = "https://github.com/choener/ViennaRNA-bindings";
  description = "ViennaRNA v2 bindings";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
