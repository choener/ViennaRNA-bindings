{
  description = ''
    ViennaRNA bindings for Haskell.
  '';

  inputs = {
    # NOTE Only update if we are sure that all packages can build with nixos>20.09.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    ghcicabal = { url = "github:choener/ghcicabal"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs = { self, nixpkgs, flake-utils, ghcicabal
  }: let
    overlay = final: prev: {
      haskellPackages = (prev.haskellPackages.override{ overrides= hself: hsuper: let
          checked   = a: hself.callHackageDirect a {};
          unchecked = a: final.haskell.lib.dontCheck (checked a);
          unb       = a: final.haskell.lib.dontCheck (final.haskell.lib.unmarkBroken a);
        in {
        };
      }).extend ( hself: hsuper: {
        ViennaRNA-bindings = hself.callPackage ./. {};
        #
      });
    };
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; overlays = [ ghcicabal.overlay self.overlay ]; };
      sharedBuildInputs = with pkgs; [ llvm_9 ];
    in {
      # update dependencies via mr, develop the package, push changes, and update the flake
      # dependencies if major changes were made or before releasing
      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [
          p.ViennaRNA-bindings
        ];
        withHoogle = true;
        buildInputs = with pkgs; [
          cabal-install
          pkgs.ghcicabal # be explicit to get the final package
          haskellPackages.haskell-language-server
          haskellPackages.hls-tactics-plugin
          nodejs # required for lsp
        ] ++ sharedBuildInputs;
      }; # devShell
    }) // { inherit overlay; };
}
