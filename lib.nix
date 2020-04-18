{pkgs}:

{
  checkWithGtkDeps = drv:
    pkgs.haskell.lib.addBuildDepends
      (pkgs.haskell.lib.overrideCabal drv (_old: {
          checkPhase = ''
            runHook preCheck
            xvfb-run dbus-run-session \
                --config-file=${pkgs.dbus.daemon}/share/dbus-1/session.conf \
                ./Setup test
            runHook postCheck
            '';
      }))
      (with pkgs; [xvfb_run dbus.daemon]);
}
