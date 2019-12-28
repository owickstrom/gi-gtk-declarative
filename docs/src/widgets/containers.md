# Containers

Containers are widgets that may contain multiple child widgets. Some
examples of such widgets are `Box`, `ListBox`, and `Paned`.

Just like single widgets and bins, the `container` function takes as
arguments a widget constructor and a list of
[properties](../attributes/properties.md). The third argument however,
is a collection of child widgets.

## Container and Children Types

Different container widgets use different types to help you construct
a valid widget hierarchy. These types are enforcing the existing GTK+
widget rules, that otherwise would be printed as warnings or errors.

Things that vary between container widget types are:

* The types of their child widgets
* The type of collection used to pass the children as a parameter

## Box

In the case of `Box`, the collection of child widgets has type
`[BoxChild event]`.

``` haskell
container Box []
  [ BoxChild defaultBoxChildProperties (widget Button [])
  , BoxChild defaultBoxChildProperties (widget CheckButton [])
  ]
```

As `BoxChildProperties` is a record, it's easy to override the
defaults with custom values, specifying how the child should be
_packed_ in the box.

``` haskell
container Box []
  [ BoxChild defaultBoxChildProperties { padding = 10 }
      (widget Button [])
  , BoxChild defaultBoxChildProperties { expand = True }
      (widget CheckButton [])
  ]
```

For convenience, widgets can be wrapped in `BoxChild` values
automatically using the default box packing properties.

``` haskell
container Box []
  [ widget Button []
  , widget CheckButton []
  ]
```

## ListBox

The collection of child widgets used with `ListBox` is of type `[Bin
ListBoxRow Widget event]`, where `ListBoxRow` is the regular
constructor defined in the [gi-gtk][] package. Instead of accepting
any `[Widget event]`, the type constrains its usage to only accept
proper `ListBoxRow` widgets as children.

``` haskell
container ListBox []
  [ bin ListBoxRow [] (widget Button [])
  , bin ListBoxRow [] (widget CheckButton [])
  ]
```

## Paned

The `Paned` widget in GTK+ has two panes, which contain one widget
each. While `Paned` widgets can be constructed using the `container`
function, the smart constructor `paned` is recommended. It takes a
list of attributes, along with two `Pane` values.

``` haskell
paned
  [#wideHandle := True]
  (pane defaultPaneProperties { resize = True } $
    widget Label [#label := "Left"])
  (pane defaultPaneProperties { resize = True, shrink = False } $
    widget Label [#label := "Right"])
```

Each `Pane` is constructed using the `pane` function, which takes a
`PaneProperties` value and a child widget. These pane property values
are used to call the underlying `pack1`/`pack2` functions on the GTK+
widget.

## Notebook

The `Notebook` widget has multiple children - called "pages" - and
tabs that allow you to view one page at a time. You should construct
Notebooks with the `notebook`, `page`, and `pageWithTab` functions.

``` haskell
notebook []
  [ page "First tab label" (widget Label [#label := "First tab content..."])
  , pageWithTab
      (widget Button [#label := "Using a button as the tab label widget"])
      (widget Label [#label := "Second tab content..."])
  ]
```


[gi-gtk]: https://hackage.haskell.org/package/gi-gtk
