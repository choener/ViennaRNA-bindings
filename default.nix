{ mkDerivation, array, base, bytestring, data-default-class
, inline-c, lib
}:
mkDerivation {
  pname = "ViennaRNA-bindings";
  version = "0.233.2.1";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring data-default-class inline-c
  ];
  homepage = "https://github.com/choener/ViennaRNA-bindings";
  description = "ViennaRNA v2 bindings";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
