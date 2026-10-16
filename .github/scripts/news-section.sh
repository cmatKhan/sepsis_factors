#!/usr/bin/env bash
# Print the NEWS.md section of a package version (the lines after its "# <package> <version>"
# heading, up to the next top-level heading). Exits 1 if the heading is missing.
# Usage: news-section.sh <package> <version> [<git-ref>]
set -euo pipefail
pkg=$1
ver=$2
ref=${3:-}
if [ -n "$ref" ]; then
  news=$(git show "$ref:$pkg/NEWS.md")
else
  news=$(cat "$pkg/NEWS.md")
fi
heading="# $pkg $ver"
if ! grep -qxF "$heading" <<<"$news"; then
  echo "NEWS.md of $pkg has no heading '$heading'" >&2
  exit 1
fi
awk -v h="$heading" '$0 == h {on=1; next} /^# / && on {exit} on' <<<"$news"
