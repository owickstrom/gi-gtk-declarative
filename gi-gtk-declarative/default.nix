{ pkgs ? import ../nixpkgs.nix, compiler ? "ghc883"
, doCheck ? false, doBenchmark ? false
}:

let
  lib = import ../lib.nix { inherit pkgs; };
  haskellPackages = pkgs.haskell.packages.${compiler};
  applyCheck = if doCheck then pkgs.lib.id else pkgs.haskell.lib.dontCheck;
  applyBench = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = lib.checkWithGtkDeps
    (applyCheck (applyBench (haskellPackages.callCabal2nix "gi-gtk-declarative" ./. {})));
in
{
  gi-gtk-declarative = drv;
}
