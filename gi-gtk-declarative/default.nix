{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865"
, doCheck ? false, doBenchmark ? false
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      criterion = self.callHackage "criterion" "1.5.3.0" {};
      hedgehog = self.callHackage "hedgehog" "1.0" {};
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
    };
  };
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
