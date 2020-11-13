{ pkgs ? <nixpkgs>, compiler ? null }:

# nixos 19-03

with import pkgs {};

let
  hsp = if compiler==null then haskellPackages else haskell.packages."${compiler}";
  hsPkgs0 = hsp.override {
  };
  #
  sourceOverrides = haskell.lib.packageSourceOverrides {
    ViennaRNA-bindings  = ./.;
  }; # haskellPackages override
  hsPkgs = (hsPkgs0.extend sourceOverrides).extend (hself: hsuper: {
    ViennaRNA-bindings = haskell.lib.dontCheck (hsuper.ViennaRNA-bindings);
  });
  # my own little tool
  cabalghcisrc =
    let local = ~/Documents/University/active/ghcicabal;
    in  if builtins.pathExists local
        then local
        else builtins.fetchGit {
          url = https://github.com/choener/ghcicabal;
          ref = "master";
        };
  cabalghci = hsPkgs.callPackage cabalghcisrc {};

in

hsPkgs.shellFor {
  packages = p: [
    p.ViennaRNA-bindings
  ];
  withHoogle = true;
  buildInputs = [
    cabal-install
    ({ "ghc8102" = llvm_9; }.compiler or llvm)
    cabalghci
    hsPkgs.alex
    hsPkgs.happy
    hsPkgs.c2hs
  ];
} # shellFor

