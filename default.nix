{ mkDerivation, alex, array, base, bytestring, c2hs
, data-default-class, happy, inline-c, stdenv
}:
mkDerivation {
  pname = "ViennaRNA-bindings";
  version = "0.233.2.1";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring data-default-class inline-c
  ];
  libraryToolDepends = [ alex c2hs happy ];
  homepage = "https://github.com/choener/ViennaRNA-bindings";
  description = "ViennaRNA v2 bindings";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
