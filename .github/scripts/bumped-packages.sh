#!/usr/bin/env bash
# Print "<package> <old version> <new version>" for each package whose DESCRIPTION Version differs
# between two git refs (old version is empty for a new package).
# Usage: bumped-packages.sh <base-ref> <head-ref>
set -euo pipefail
base=$1
head=$2
for pkg in sepsisfactors sepsisfactorsdb sepsisfactorsapp; do
  new=$(git show "$head:$pkg/DESCRIPTION" | sed -n 's/^Version:[[:space:]]*//p')
  old=$(git show "$base:$pkg/DESCRIPTION" 2>/dev/null | sed -n 's/^Version:[[:space:]]*//p' || true)
  if [ "$new" != "$old" ]; then
    echo "$pkg $old $new"
  fi
done
