#! /bin/bash
set -euo pipefail

hpack cooked-validators/
hpack examples/

cd cooked-validators
cabal run tests

cd examples
cabal run tests
