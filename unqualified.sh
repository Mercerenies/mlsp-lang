#!/bin/bash

# Checks for unqualified Haskell modules (modules which do not explicitly list
# exports)
find . -name '*.hs' | xargs perl -ne 'print $ARGV . "\n" if /^module [\w.]+ where/'
