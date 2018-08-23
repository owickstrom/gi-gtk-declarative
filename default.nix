{ pkgs ? import <nixpkgs> {}, compiler ? "ghc843", doBenchmark ? false }:
{
  gi-gtk-declarative = (import ./gi-gtk-declarative { inherit compiler; }).gi-gtk-declarative;
  examples = (import ./examples { inherit compiler; }).examples;
}
