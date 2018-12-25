{ pkgs ? import <nixpkgs> {}, compiler ? "ghc862", doBenchmark ? true }:
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  project = import ./. { inherit compiler doBenchmark; };
in
  haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [
      project.gi-gtk-declarative
      project.gi-gtk-declarative-app-simple
      project.examples
    ];
  }
