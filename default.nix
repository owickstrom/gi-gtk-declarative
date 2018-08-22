{ pkgs ? import <nixpkgs> {}, compiler ? "ghc843", doBenchmark ? false }:

let

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
      indexed-extras = self.callPackage ./indexed-extras.nix {};
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callCabal2nix "gi-gtk-declarative" ./. {});
in
  if pkgs.lib.inNixShell then drv.env else drv
