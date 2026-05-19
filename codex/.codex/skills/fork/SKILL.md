---
name: fork
description: "Create an isolated Git worktree from the current repository state, check out a new branch at the current HEAD, start a tmux session in that worktree, and launch a forked Codex session there. Use when the user asks to fork the current Codex work, split off a parallel task, create a new worktree/branch/tmux Codex session, or continue the current context in an isolated workspace."
---

# Fork

## Quick Start

Use `scripts/fork_worktree.py` from the skill directory to create the workspace and launch Codex:

```bash
python3 ~/.codex/skills/fork/scripts/fork_worktree.py --name <short-task-name>
```

By default, the script:

1. Resolves the current Git repository and current `HEAD`.
2. Creates a new branch at that commit.
3. Adds a new worktree for that branch.
4. Starts a detached tmux session with its working directory set to the worktree.
5. Runs `codex fork --yolo --last --all -C <worktree>` inside tmux.

Report the resulting worktree path, branch name, tmux session name, and attach command to the user.

## Workflow

Run from the repository or worktree the user wants to fork. If the user gives a task name, pass it with `--name`; otherwise let the script generate a timestamped name.

Prefer the default `codex fork --yolo --last --all` behavior when the user says to fork "this" or "the current" session. `--all` avoids cwd filtering after switching into the new worktree. Pass `--session-id <uuid>` only when the user gives a specific Codex session id.

Use `--branch <branch-name>` when the user requests an exact branch name. Use `--path <worktree-path>` when the user requests an exact worktree location.

If the source worktree has uncommitted changes, stop and ask whether to commit, stash, or continue from committed `HEAD` only. To continue from `HEAD` while leaving source changes behind, rerun with `--allow-dirty-source`.

## Examples

Create a named fork:

```bash
python3 ~/.codex/skills/fork/scripts/fork_worktree.py --name fix-login-tests
```

Create a fork with an exact branch and worktree path:

```bash
python3 ~/.codex/skills/fork/scripts/fork_worktree.py --branch codex/fix-login-tests --path ../repo-fix-login-tests
```

Fork a specific Codex session id:

```bash
python3 ~/.codex/skills/fork/scripts/fork_worktree.py --name api-refactor --session-id 00000000-0000-0000-0000-000000000000
```

Preview without making changes:

```bash
python3 ~/.codex/skills/fork/scripts/fork_worktree.py --name dry-run-example --dry-run
```
