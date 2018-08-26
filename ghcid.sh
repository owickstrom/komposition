#!/usr/bin/env bash

TASTY_COLOR=always \
  ghcid \
  -c 'ghci -isrc -iexe -itest -idist/build/autogen -fobject-code' \
  --reload=src \
  --reload=exe \
  --reload=test \
  exe/Main.hs \
  $@

