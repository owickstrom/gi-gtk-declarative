{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc884", doBenchmark ? true, doCheck ? true }:
let
  fontsConf = pkgs.makeFontsConf {
    fontDirectories = [ pkgs.cantarell-fonts ];
  };
  haskellPackages = pkgs.haskell.packages.${compiler};
  project = import ./. { inherit compiler doBenchmark doCheck pkgs; };
in
  haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [
      project.gi-gtk-declarative
      project.gi-gtk-declarative-app-simple
      project.examples
    ];
    buildInputs = [
      project.docs
      pkgs.cabal-install
    ];
    FONTCONFIG_FILE = fontsConf;
  }
