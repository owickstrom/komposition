#!/usr/bin/env bash

TASTY_COLOR=always \
  ghcid \
  -c 'ghci -isrc -iexe -itest -idist-newstyle/build/x86_64-linux/ghc-8.4.3/komposition-0.1.0/build/autogen -fobject-code' \
  --reload=src \
  --reload=exe \
  --reload=test \
  exe/Main.hs \
  $@



