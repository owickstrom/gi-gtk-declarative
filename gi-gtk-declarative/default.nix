{ pkgs ? import <nixpkgs> {}, compiler ? "ghc862"
, doCheck ? false, doBenchmark ? false
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      criterion = self.callHackage "criterion" "1.5.3.0" {};
      hedgehog = self.callHackage "hedgehog" "1.0" {};
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
    };
  };
  applyCheck = if doCheck then pkgs.haskell.lib.doCheck else pkgs.lib.id;
  applyBench = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = applyCheck (applyBench (haskellPackages.callCabal2nix "gi-gtk-declarative" ./. {}));
in
{
  gi-gtk-declarative = drv;
}
