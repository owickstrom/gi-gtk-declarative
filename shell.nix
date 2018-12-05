{ pkgs ? import <nixpkgs> {}, compiler ? "ghc844", doBenchmark ? false }:
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  project = import ./. { inherit compiler; };
in
  haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [
      project.gi-gtk-declarative
      project.gi-gtk-declarative-app-simple
      project.examples
    ];
  }
