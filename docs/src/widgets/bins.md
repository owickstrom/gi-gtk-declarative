# Bins

Bins are widgets with exactly one child widget. They are constructed
using the `bin` function, given a GTK+ widget constructor, a list of
attributes, and a child widget.

``` haskell
bin Window [#title := "Example Window"] $
  widget Label [#label := "Nothing here yet."]
```

In some cases, specific types of bins are used as children of other
widgets. In the case of `ListBox`, we use `ListBoxRow` bins as
children.

``` haskell
container ListBox []
  [ bin ListBoxRow [] $ widget Button []
  , bin ListBoxRow [] $ widget CheckButton []
  ]
```

To find which GTK+ widgets are bins, use the [Haddock
documentation of gi-gtk][gi-gtk] and look for objects with instances of the
`IsBin` type class.


[gi-gtk]: https://hackage.haskell.org/package/gi-gtk
