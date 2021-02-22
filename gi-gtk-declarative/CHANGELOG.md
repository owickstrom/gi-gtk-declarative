* 0.7.0
    - Version bounds compatibility with Stack resolver lts-17.0
    - Replace Travis badge with a Github workflow one.
    - Replace .travis.yml with a Github Actions Workflow.
    - Improved exception handling and async handling in app-simple
    - Fix race condition in app-simple
    - Fix patching of grid child properties.
* 0.6.3
    - Add `Grid` container widget
    - Fix bugs in patching properties for all types of widgets
* 0.6.2
    - Add `Notebook` container widget
* 0.6.1
    - Fix Nix build issue
* 0.6.0
    - Allow dependency haskell-gi-0.23
    - Remove redundant code
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
