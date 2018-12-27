<div align="center">
  <h1>gi-gtk-declarative</h1>
  <p>Declarative GTK+ programming in Haskell</p>
  <p>
    <a href="https://hackage.haskell.org/package/gi-gtk-declarative"><img src="https://img.shields.io/hackage/v/gi-gtk-declarative.svg?style=flat" alt="Hackage"></a>
    <a href="https://hackage.haskell.org/package/gi-gtk-declarative-app-simple"><img src="https://img.shields.io/hackage/v/gi-gtk-declarative-app-simple.svg?style=flat" alt="Hackage"></a>
    <a href="https://travis-ci.org/owickstrom/gi-gtk-declarative"><img src="https://travis-ci.org/owickstrom/gi-gtk-declarative.svg?branch=master" alt="Build Status"></a>
  </p>
  <p>
    <a href="https://owickstrom.github.io/gi-gtk-declarative/">Documentation</a>
  </p>
</div>

## Usage

Learn how to use this package at [the documentation website](https://owickstrom.github.io/gi-gtk-declarative/).

## Build Instructions

Using newer versions of Cabal, run:

```
cabal new-build all
```

Or using Stack:

```
stack build
```

You may also use Nix:

```
nix-shell
```

The documentation is built using [MkDocs](https://www.mkdocs.org/).

## Examples

There are some examples in [examples/](examples/), using the
`GI.Gtk.Declarative.App.Simple` architecture, which also showcase
`GI.Gtk.Declarative` (the markup library.)

As an example, to run the `examples/Hello.hs` example, follow these steps
(assuming you have a recent version of Cabal):

``` shell
cabal new-run example Hello
```

You might also build in a Cabal sandbox, using Stack, or with Nix.

## Status

**EXPERIMENTAL!** Do not bet your business on this quite yet.

## Requirements

Follow the installation instructions at
[haskell-gi](https://github.com/haskell-gi/haskell-gi#installation) to
make sure you have the required GObject and GTK+ libraries installed.

As this package relies heavily on `OverloadedLabels` and the
`haskell-gi-overloading` functionality, the restrictions apply [as
described in the haskell-gi package
documentation](https://github.com/haskell-gi/haskell-gi), i.e. you
cannot compile this package with GHC 8.2.x.

## License

Copyright 2018 Ⓒ Oskar Wickström

[Mozilla Public License Version 2.0](LICENSE)
