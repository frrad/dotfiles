#!/usr/bin/env bash
set -euo pipefail

stow_bin=xstow
if ! command -v "$stow_bin" >/dev/null 2>&1; then
  stow_bin=stow
fi

failed=0
for f in git emacs zsh ssh sqlite screen ispell claude
do
  output=$("$stow_bin" "$f" 2>&1) || {
    # Conflicts with existing non-link targets are expected in CI
    # where tools like git create default configs before stow runs.
    if echo "$output" | grep -q "since neither a link nor a directory"; then
      echo "warning: stow $f skipped (existing target conflict)" >&2
    else
      echo "error: failed to stow $f:" >&2
      echo "$output" >&2
      failed=1
    fi
  }
done
exit "$failed"
