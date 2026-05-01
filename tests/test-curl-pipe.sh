#!/usr/bin/env bash
# Reproduces the `curl ... | sudo bash` failure.
#
# When bootstrap.sh is run via stdin (as curl|bash does), BASH_SOURCE[0] is
# empty, so resolve_script_dir resolves to CWD rather than the dotfiles
# directory.  packages.tsv is then looked up in CWD and not found.
#
# Usage (must be run as root or via sudo):
#   sudo bash tests/test-curl-pipe.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [ -z "${SUDO_USER:-}" ]; then
  echo "must be run via sudo (SUDO_USER must be set)" >&2
  exit 1
fi

workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT

echo "=== Reproducing curl|bash failure (CWD: $workdir) ==="
echo "    bootstrap.sh is at: $REPO_ROOT/bootstrap.sh"
echo "    packages.tsv NOT present in $workdir"
echo ""

# Run from a directory that does NOT contain packages.tsv, reading the
# script from stdin — the same execution model as `curl ... | sudo bash`.
if (cd "$workdir" && bash < "$REPO_ROOT/bootstrap.sh") 2>&1 | grep -q "packages.tsv: No such file"; then
  echo "PASS: reproduced expected failure (packages.tsv not found in CWD)"
  exit 0
else
  echo "FAIL: did not reproduce the expected error" >&2
  exit 1
fi
