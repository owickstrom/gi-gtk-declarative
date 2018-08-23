{ pkgs ? import <nixpkgs> {}, compiler ? "ghc843", doBenchmark ? false }:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      gi-gtk-declarative = (import ../gi-gtk-declarative { inherit compiler; }).gi-gtk-declarative;
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callCabal2nix "examples" ./. {});
in
{
  examples = drv;
}
