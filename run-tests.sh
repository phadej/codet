#!/bin/sh

set -ex

cabal run codet-tests -w ghc-9.0.2 -- "$@"
cabal run codet-tests -w ghc-9.2.8 -- "$@"
cabal run codet-tests -w ghc-9.8.1 -- "$@"

cabal run codet-plugin-tests -w ghc-9.0.2 -- "$@"
cabal run codet-plugin-tests -w ghc-9.2.8 -- "$@"
cabal run codet-plugin-tests -w ghc-9.8.1 -- "$@"
