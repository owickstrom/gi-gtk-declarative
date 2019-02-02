# Build Instructions

To hack on and build these libraries, using newer versions of Cabal,
run:

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
