#!/usr/bin/env bash

export HEDGEHOG_COLOR=1

ghcid \
  -c 'ghci -igi-gtk-declarative/src -igi-gtk-declarative/test' \
  --test ':main' \
  --color=always \
  --reload=gi-gtk-declarative \
  --restart=gi-gtk-declarative/test \
  Main


