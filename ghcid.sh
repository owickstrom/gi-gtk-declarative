#!/usr/bin/env bash

ghcid \
  -c 'ghci -igi-gtk-declarative/src -igi-gtk-declarative-app-simple/src -iexamples examples/Main.hs' \
  --reload=gi-gtk-declarative \
  --reload=gi-gtk-declarative-app-simple \
  --reload=examples


