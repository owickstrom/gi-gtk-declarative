# CSS

CSS classes are declared using the `classes` function in the
attributes list. It's a set of text values that is added to the
underlying GTK+ widget's style context as class names.

``` haskell
widget
  Button
  [ classes ["big-button"]
  , #label := "CLICK ME"
  ]
```

GTK+ supports a subset of CSS. To learn more, see the [GTK+ CSS
Overview](https://developer.gnome.org/gtk3/stable/chap-css-overview.html).

!!! note

    To have any effect, a CSS provider needs to be set up
    for the GDK screen. See [the CSS example](https://github.com/owickstrom/gi-gtk-declarative/blob/master/examples/CSS.hs) in the repository for a demonstration.
