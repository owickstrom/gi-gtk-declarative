{ pkgs ? import <nixpkgs> {}, compiler ? "ghc843", doBenchmark ? false }:
let haskellPackages = pkgs.haskell.packages.${compiler};
in
  haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [
      (import ./gi-gtk-declarative { inherit compiler; }).gi-gtk-declarative
      (import ./examples { inherit compiler; }).examples
    ];
  }
