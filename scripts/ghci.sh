#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# We use a ghci.conf file instead of loading directly bin/humming.hs. This lets
# us add additional modules for convenience when trying expressions inside
# GHCi.

ghc --interactive \
  -haddock \
  -ibin/ \
  -iDatabase/ \
  -iHumming/ \
  -Wall \
  -Wno-type-defaults \
  -ghci-script scripts/ghci.conf
