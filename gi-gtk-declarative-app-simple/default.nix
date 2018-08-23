{ pkgs ? import <nixpkgs> {}, compiler ? "ghc843", doBenchmark ? false }:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
      gi-gtk-declarative = (import ../gi-gtk-declarative { inherit compiler; }).gi-gtk-declarative;
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callCabal2nix "gi-gtk-declarative-app-simple" ./. {});
in
{
  gi-gtk-declarative-app-simple = drv;
}
