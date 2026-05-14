#!/usr/bin/env python3
"""Generate packages.tsv from packages.toml.

Run after editing packages.toml:
    python3 generate_packages.py > packages.tsv
"""

import sys
from pathlib import Path

try:
    import tomllib
except ImportError:
    try:
        import tomli as tomllib  # type: ignore[no-redef]
    except ImportError:
        sys.exit("Python 3.11+ or 'pip install tomli' required")

HEADER = "# name|apt_check_cmd|brew_check_cmd|apt_packages|brew_packages"


def resolve(name: str, spec: dict) -> tuple[str, str, str, str, str]:
    default_cmd = spec.get("cmd", name)

    apt = spec.get("apt", name)
    brew = spec.get("brew", name)

    if apt is False:
        apt_cmd = ""
        apt_pkg = ""
    else:
        apt_cmd = spec.get("apt_cmd", default_cmd)
        apt_pkg = " ".join(apt) if isinstance(apt, list) else apt

    if brew is False:
        brew_cmd = ""
        brew_pkg = ""
    else:
        brew_cmd = spec.get("brew_cmd", default_cmd)
        brew_pkg = brew

    return name, apt_cmd, brew_cmd, apt_pkg, brew_pkg


def main() -> None:
    toml_path = Path(__file__).parent / "packages.toml"
    with open(toml_path, "rb") as f:
        data = tomllib.load(f)

    rows = sorted(
        (resolve(name, spec) for name, spec in data.items()),
        key=lambda r: r[0],
    )

    print(HEADER)
    for row in rows:
        print("|".join(row))


if __name__ == "__main__":
    main()
