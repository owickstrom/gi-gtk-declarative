{ pkgs ? import ../nixpkgs.nix }:
let
  mach-nix = import (builtins.fetchGit {
    url = "https://github.com/DavHau/mach-nix";
    ref = "refs/tags/3.5.0";
  }) { pkgs = pkgs; };
in mach-nix.mkPython { requirements = builtins.readFile ./requirements.txt; }
