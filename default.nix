{ pkgs ? import <nixpkgs> {}, compiler ? "ghc862", doBenchmark ? false }:
rec {
  docs = import ./docs/requirements.nix { inherit pkgs; };
  gi-gtk-declarative = (import ./gi-gtk-declarative { inherit compiler doBenchmark; }).gi-gtk-declarative;
  gi-gtk-declarative-app-simple = (import ./gi-gtk-declarative-app-simple { inherit compiler doBenchmark gi-gtk-declarative; }).gi-gtk-declarative-app-simple;
  examples = (import ./examples { inherit compiler doBenchmark gi-gtk-declarative gi-gtk-declarative-app-simple; }).examples;
}
