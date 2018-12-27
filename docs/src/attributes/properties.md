# Properties

GTK+ widgets, being built on the GObject framework, have
properties. These are key/value pairs that can be get and set
generically. This package uses `OverloadedLabels` to declare
properties and their values for GTK+ widgets.  There are many
properties available for GTK+ widgets. To find them, use the
[gi-gtk][] documentation. Each widget module lists its properties in
the bottom of the Haddock page.

## Properties in the Attributes List

In gi-gtk-declarative, the list passed to widgets is not a list of
properties, but a list of _attributes_. The attributes list include
property declarations, [events](events.md), and [CSS classes](css.md).
To declare a property and a value in the attributes list, we use the
`(:=)` operator.

Here we construct a button with a specific text label:

``` haskell
widget Button [#label := "Click Here"]
```

In the following example we declare a `ScrolledWindow`, and set the
horizontal scroll bar policy to _automatically_ decide whether the
scroll bar should be visible:

``` haskell
bin ScrolledWindow [ #hscrollbarPolicy := PolicyTypeAutomatic ]
  someSuperWideWidget
```

As a final example, we declare a `ListBox` with the multiple selection
mode enabled:

``` haskell
container ListBox [ #selectionMode := SelectionModeMultiple ]
  children
```


[gi-gtk]: https://hackage.haskell.org/package/gi-gtk
