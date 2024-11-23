
{
  description = "RAFFLEIZE";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP }:
    let
      supportedSystems = [
        "x86_64-linux"	"x86_64-darwin"	"aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlay = final: prev: {
          haskell-nix = prev.haskell-nix // {
            extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
                # String pkgconfig-depends names are mapped to lists of Nixpkgs
                # package names
                "libblst" = [ "blst" ];
            };
          };
        };

        overlays = [ haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc966";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = {}  ;
                  hlint = {};
                  haskell-language-server = {};
                };
                # Non-Haskell applications required by raffleize
                shell.buildInputs = with pkgs; [
                  haskellPackages.cabal-fmt
                  haskellPackages.fourmolu
                  just
                  nixpkgs-fmt
                  nix-prefetch-git
                ];
                inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
              };
          })
          overlay
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com" # No longer needed
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=" # No longer needed
    ];
  };
}
