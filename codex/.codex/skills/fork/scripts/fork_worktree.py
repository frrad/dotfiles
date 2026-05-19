#!/usr/bin/env python3
"""Create a Git worktree, start tmux there, and launch a forked Codex session."""

import argparse
import datetime
import os
import re
import shlex
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class ForkConfig:
    name: str
    branch: str
    worktree_path: Path
    tmux_session: str
    base_ref: str
    session_id: str | None
    allow_dirty_source: bool
    dry_run: bool


def run_git(args: list[str], cwd: Path) -> str:
    result = subprocess.run(
        ["git", *args],
        cwd=cwd,
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return result.stdout.strip()


def require_command(command: str) -> None:
    if shutil.which(command) is None:
        raise RuntimeError(f"Required command not found on PATH: {command}")


def slugify(value: str) -> str:
    slug = re.sub(r"[^A-Za-z0-9._-]+", "-", value.strip().lower())
    slug = re.sub(r"-+", "-", slug).strip("-._")
    if not slug:
        raise RuntimeError("Name must contain at least one letter or digit")
    return slug[:64]


def tmux_safe_name(value: str) -> str:
    name = re.sub(r"[^A-Za-z0-9_-]+", "-", value)
    name = re.sub(r"-+", "-", name).strip("-_")
    if not name:
        raise RuntimeError("tmux session name must contain at least one letter or digit")
    return name[:80]


def generated_name(head: str) -> str:
    timestamp = datetime.datetime.now(datetime.UTC).strftime("%Y%m%d-%H%M%S")
    return f"codex-{timestamp}-{head[:8]}"


def default_worktree_parent(repo_root: Path) -> Path:
    parts = repo_root.parts
    for index in range(len(parts) - 2):
        if parts[index] == ".claude" and parts[index + 1] == "worktrees":
            return Path(*parts[: index + 2])

    claude_worktrees = repo_root / ".claude" / "worktrees"
    if claude_worktrees.exists():
        return claude_worktrees

    dot_worktrees = repo_root / ".worktrees"
    if dot_worktrees.exists():
        return dot_worktrees

    return repo_root.parent / f"{repo_root.name}-worktrees"


def unique_branch(base_branch: str, cwd: Path) -> str:
    existing = set(run_git(["for-each-ref", "--format=%(refname:short)", "refs/heads"], cwd).splitlines())
    if base_branch not in existing:
        return base_branch
    for suffix in range(2, 1000):
        candidate = f"{base_branch}-{suffix}"
        if candidate not in existing:
            return candidate
    raise RuntimeError(f"Could not find an unused branch name based on {base_branch}")


def unique_path(path: Path) -> Path:
    if not path.exists():
        return path
    for suffix in range(2, 1000):
        candidate = path.with_name(f"{path.name}-{suffix}")
        if not candidate.exists():
            return candidate
    raise RuntimeError(f"Could not find an unused worktree path based on {path}")


def unique_tmux_session(base_session: str) -> str:
    result = subprocess.run(
        ["tmux", "list-sessions", "-F", "#{session_name}"],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    existing = set(result.stdout.splitlines()) if result.returncode == 0 else set()
    if base_session not in existing:
        return base_session
    for suffix in range(2, 1000):
        candidate = f"{base_session}-{suffix}"
        if candidate not in existing:
            return candidate
    raise RuntimeError(f"Could not find an unused tmux session name based on {base_session}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Create a Git worktree, new branch, tmux session, and forked Codex session."
    )
    parser.add_argument("--name", help="Short task name used for generated branch, path, and tmux names.")
    parser.add_argument("--branch", help="Exact branch name to create. Fails if it already exists.")
    parser.add_argument("--path", type=Path, help="Exact worktree path to create.")
    parser.add_argument("--tmux-session", help="Exact tmux session name. Fails if it already exists.")
    parser.add_argument("--session-id", help="Specific Codex session UUID to fork. Defaults to codex fork --last.")
    parser.add_argument(
        "--allow-dirty-source",
        action="store_true",
        help="Allow forking from committed HEAD even when the source worktree has uncommitted changes.",
    )
    parser.add_argument("--dry-run", action="store_true", help="Print planned actions without changing anything.")
    return parser.parse_args()


def build_config(args: argparse.Namespace, cwd: Path) -> ForkConfig:
    require_command("git")
    require_command("tmux")
    require_command("codex")

    repo_root = Path(run_git(["rev-parse", "--show-toplevel"], cwd)).resolve()
    head = run_git(["rev-parse", "HEAD"], repo_root)
    name = slugify(args.name) if args.name else generated_name(head)

    if args.branch:
        branch = args.branch
    else:
        branch = unique_branch(f"codex/{name}", repo_root)

    if args.path:
        worktree_path = args.path.expanduser().resolve()
    else:
        worktree_path = unique_path(default_worktree_parent(repo_root) / name)

    if args.tmux_session:
        tmux_session = args.tmux_session
    else:
        tmux_session = unique_tmux_session(f"codex-{tmux_safe_name(name)}")

    return ForkConfig(
        name=name,
        branch=branch,
        worktree_path=worktree_path,
        tmux_session=tmux_session,
        base_ref=head,
        session_id=args.session_id,
        allow_dirty_source=args.allow_dirty_source,
        dry_run=args.dry_run,
    )


def check_source_clean(cwd: Path, allow_dirty_source: bool) -> None:
    status = run_git(["status", "--porcelain=v1"], cwd)
    if status and not allow_dirty_source:
        raise RuntimeError(
            "Source worktree has uncommitted changes. Commit or stash them, or rerun with "
            "--allow-dirty-source to fork from committed HEAD only."
        )


def create_worktree(config: ForkConfig, cwd: Path) -> None:
    if config.dry_run:
        return

    if config.branch in run_git(["for-each-ref", "--format=%(refname:short)", "refs/heads"], cwd).splitlines():
        raise RuntimeError(f"Branch already exists: {config.branch}")
    if config.worktree_path.exists():
        raise RuntimeError(f"Worktree path already exists: {config.worktree_path}")

    config.worktree_path.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        ["git", "worktree", "add", "-b", config.branch, str(config.worktree_path), config.base_ref],
        cwd=cwd,
        check=True,
    )


def start_tmux(config: ForkConfig) -> list[str]:
    codex_command = ["codex", "fork"]
    if config.session_id:
        codex_command.extend(["-C", str(config.worktree_path), config.session_id])
    else:
        codex_command.extend(["--last", "--all", "-C", str(config.worktree_path)])

    shell_command = f"exec {shlex.join(codex_command)}"
    tmux_command = [
        "tmux",
        "new-session",
        "-d",
        "-s",
        config.tmux_session,
        "-c",
        str(config.worktree_path),
        shell_command,
    ]

    if not config.dry_run:
        subprocess.run(tmux_command, check=True)

    return tmux_command


def print_summary(config: ForkConfig, tmux_command: list[str]) -> None:
    print(f"Name: {config.name}")
    print(f"Branch: {config.branch}")
    print(f"Base HEAD: {config.base_ref}")
    print(f"Worktree: {config.worktree_path}")
    print(f"tmux session: {config.tmux_session}")
    print(f"tmux attach: tmux attach -t {shlex.quote(config.tmux_session)}")
    print(f"tmux command: {shlex.join(tmux_command)}")
    if config.dry_run:
        print("Dry run: no changes made")


def main() -> int:
    args = parse_args()
    cwd = Path.cwd()
    try:
        config = build_config(args, cwd)
        check_source_clean(cwd, config.allow_dirty_source)
        create_worktree(config, cwd)
        tmux_command = start_tmux(config)
        print_summary(config, tmux_command)
    except (RuntimeError, subprocess.CalledProcessError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
