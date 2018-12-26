{ pkgs ? import <nixpkgs> {}, compiler ? "ghc862", doBenchmark ? false }:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      criterion = self.callHackage "criterion" "1.5.3.0" {};
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callCabal2nix "gi-gtk-declarative" ./. {});
in
{
  gi-gtk-declarative = drv;
}
