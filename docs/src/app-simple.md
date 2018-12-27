# App.Simple

The _App.Simple_ framework, provided by the
[gi-gtk-declarative-app-simple][] package, build on gi-gtk-declarative
to provide a simple application architecture. It's based on a state
reducer, inspired by [PureScript's
Pux](http://purescript-pux.org/). It also draws inspiration from with
the earlier versions of [the Elm
architecture](https://guide.elm-lang.org/architecture/).

## App

The central abstraction is the `App`:

``` haskell
data App window state event =
  App
    { update       :: state -> event -> Transition state event
    , view         :: state -> AppView window event
    , inputs       :: [Producer event IO ()]
    , initialState :: state
    }
```

Let's look at the individual parts:

* The `update` function transition from the current state, based on an
  event, to another state. It may also exit the application, using the
  `Exit` constructor of `Transition`.
* The `view` function renders the current state as a window widget.
  `AppView` is a type alias that constrains the widget to be a window
  bin widget:

        type AppView window event = Bin window Widget event

* `inputs` is a list of [Pipes][] producers, emitting events that feed
  into the state reducer loop. This is useful to emit events at regular
  intervals or from some external source.
* The `initialState` is the state value that we begin with.


## Running the Application

The easiest way to run an `App` is the `run` function. Given that we
have the following definitions:

``` haskell
data MyState = ...
data MyEvent = ...

view' :: AppView Window MyEvent

update' :: MyState -> MyEvent -> Transition MyState MyEvent

clockTicks :: Producer MyEvent IO ()
```

We then define our `main` action to run an application:

``` haskell
main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = [clockTicks]
  , initialState = Initial
  }
```

As `run` returns the last state, we ignore it using `void`. Accessing
the last state can be useful if you want to embed a declarative `App`
inside a larger GTK+ application.

!!! note

    If you need more control over GTK+ initialization and the setup, for
    instance when using [CSS](attributes/css.md), use the `runLoop` function.

[gi-gtk-declarative-app-simple]: https://hackage.haskell.org/package/gi-gtk-declarative-app-simple-0.2.0
[Pipes]: http://hackage.haskell.org/package/pipes
