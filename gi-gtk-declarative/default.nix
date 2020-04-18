{ pkgs ? import ../nixpkgs.nix, compiler ? "ghc883"
, doCheck ? false, doBenchmark ? false
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  applyCheck = if doCheck then pkgs.lib.id else pkgs.haskell.lib.dontCheck;
  applyBench = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv =
    pkgs.haskell.lib.addBuildDepends
      (pkgs.haskell.lib.overrideCabal
        (applyCheck (applyBench (haskellPackages.callCabal2nix "gi-gtk-declarative" ./. {})))
        (old: {
          checkPhase = ''
            runHook preCheck
            xvfb-run dbus-run-session \
              --config-file=${pkgs.dbus.daemon}/share/dbus-1/session.conf \
              ./Setup test
            runHook postCheck
          '';
        }))
    (with pkgs; [xvfb_run dbus.daemon]);
in
{
  gi-gtk-declarative = drv;
}
