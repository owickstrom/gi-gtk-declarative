# gi-gtk-declarative

<p id="subtitle">
Declarative GTK+ programming in Haskell
</p>

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
