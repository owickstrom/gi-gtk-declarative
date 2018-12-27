# Single Widgets

Single widgets have no child widgets. They are constructed using the
`widget` function, given a GTK+ widget constructor (defined by the
[gi-gtk](https://hackage.haskell.org/package/gi-gtk) package) and a
list of attributes.

```haskell
widget Button []
```

Properties can be set on widgets with the attributes list. This is
described in more detail in [Properties](../properties.md).

``` haskell
widget Label [#label := "Hello, World!"]
```

[gi-gtk]: https://hackage.haskell.org/package/gi-gtk
