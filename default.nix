{ pkgs ? import <nixpkgs> {}, compiler ? "ghc844", doBenchmark ? false }:
{
  gi-gtk-declarative = (import ./gi-gtk-declarative { inherit compiler; }).gi-gtk-declarative;
  gi-gtk-declarative-app-simple = (import ./gi-gtk-declarative-app-simple { inherit compiler; }).gi-gtk-declarative-app-simple;
  examples = (import ./examples { inherit compiler; }).examples;
}
