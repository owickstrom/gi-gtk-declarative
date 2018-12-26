{ pkgs ? import <nixpkgs> {}, compiler ? "ghc862", doBenchmark ? false
, gi-gtk-declarative, gi-gtk-declarative-app-simple
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      inherit gi-gtk-declarative gi-gtk-declarative-app-simple;
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callCabal2nix "examples" ./. {});
in
{
  examples = drv;
}
