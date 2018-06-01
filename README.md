# gi-gtk-declarative

*Declarative GTK+ programming in Haskell.*

## Motivation & Goal

Using declarative markup to construct user interfaces, in a purely
functional fashion, is a great joy. Instead of imperatively building
up stateful objects in `IO`, you construct a regular data structure
describing the user interface to render.

In web development, declarative user interfaces are supported not only
by HTML and related standards, but more recently also by "virtual DOM"
technologies, as found in React and Elm. Web technologies have then
spread to desktop applications through embedded web views, enabled by
technologies like [Electron](https://electronjs.org/).

But what about regular (non-web) GUI frameworks for desktop, like
GTK+? We should be able to benefit from the declarative programming
model, stealing ideas from virtual DOM implementations and web front
end frameworks, while using the battle-tested native GUI technologies
on the desktop.

This is the goal of gi-gtk-declarative; a declarative and purely
functional programming model for GTK+ user interfaces. The library
aims to extend the
[haskell-gi](https://github.com/haskell-gi/haskell-gi) family of
packages as transparently as possible, not having to reimplement or
manually wrap large parts of existing GTK+ widgets.

## Status

**EXPERIMENTAL!** Do not bet your business on this quite yet.

## Requirements

As this library relies heavily on `OverloadedLabels` and the
`haskell-gi-overloading` functionality, the restrictions apply [as
described in the haskell-gi package
documentation](https://github.com/haskell-gi/haskell-gi), i.e. you
cannot compile this package with GHC 8.2.x.

## Example

There is currently one example available in
[examples/Hello.hs](examples/Hello.hs). More to come soon, hopefully.

To run the `Hello.hs` example, follow these steps:

``` shell
cabal new-build
cabal new-run gi-gtk-declarative-example-hello
```

## License

Copyright 2018 Ⓒ Oskar Wickström

[Mozilla Public License Version 2.0](LICENSE)
