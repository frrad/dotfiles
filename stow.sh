#!/usr/bin/env bash
set -euo pipefail

stow_bin=xstow
if ! command -v "$stow_bin" >/dev/null 2>&1; then
  stow_bin=stow
fi

for f in git emacs zsh ssh sqlite screen ispell
do
  if ! "$stow_bin" "$f"; then
    echo "warning: failed to stow $f; continuing" >&2
  fi
done
