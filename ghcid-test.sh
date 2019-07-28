#!/usr/bin/env bash

ghcid \
  -c 'ghci -igi-gtk-declarative/src -igi-gtk-declarative/test' \
  --test ':main' \
  --color=always \
  --reload=gi-gtk-declarative \
  Main


