* 0.5.0
  - New `CustomWidget` API:
    - easier-to-use internal state
    - pass-through attributes to top widget
* 0.4.0
    - Use `Vector` instead of `[]` for child widgets
* 0.3.0
    - Add user documentation
    - Use record for `BoxChild` properties (breaking change!)
    - Use lists for child widgets instead of `MarkupOf` monad (breaking change!)
    - Add support for `Paned` widget
* 0.2.0
    - Introduce shadow state (breaking change!)
    - Optimized patching (2x-7x faster!)
    - Many bug fixes in patching
    - Reimplement callback conversions
    - Return pairs in declarative event handlers, for non-`()` GTK+ callback return values

* 0.1.0
    - First version of `gi-gtk-declarative`!
    - Basic widget without event handling
    - Support for `Box` and `ScrolledWindow` containers
    - Declarative CSS classes
