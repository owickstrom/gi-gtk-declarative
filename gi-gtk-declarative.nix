{ mkDerivation, base, gi-gdk, gi-glib, gi-gobject, gi-gtk
, haskell-gi, haskell-gi-base, haskell-gi-overloading, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "gi-gtk-declarative";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base gi-gobject gi-gtk haskell-gi haskell-gi-base
    haskell-gi-overloading text unordered-containers
  ];
  executableHaskellDepends = [
    base gi-gdk gi-glib gi-gobject gi-gtk haskell-gi haskell-gi-base
    text
  ];
  description = "Declarative GTK+ programming in Haskell";
  license = stdenv.lib.licenses.mpl20;
}
