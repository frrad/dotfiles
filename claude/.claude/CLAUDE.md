# Global Preferences

## Git
- Always use SSH URLs for GitHub remotes (`git@github.com:...`), never HTTPS.
- Never rewrite git history in a way that requires force-pushing (no `--amend`, `rebase`, etc. on pushed commits). PRs are squash-merged, so branch history doesn't matter.
- When a feature branch is behind main, prefer merging main into the feature branch over rebasing.

## GitHub Repo Setup
- Squash merge only (disable merge commits and rebase merges).
- Branch protection (require PRs, no direct pushes, enforce for admins) — only set up on public repos. Skip for private repos (requires Pro).
- Keep repos private unless told otherwise.

## PR Review Comments
- When fixing inline comments on PRs, reply to the comment explaining what was changed, then resolve the comment thread when appropriate.
- Reply to a review comment via REST: `gh api repos/OWNER/REPO/pulls/PR_NUMBER/comments/COMMENT_ID/replies -X POST -f body="..."`. The PR number is required in the path.
- To resolve threads, use the GraphQL `resolveReviewThread` mutation. The `threadId` must be a `PRRT_...` (review thread ID), NOT a `PRRC_...` (review comment ID). Fetch thread IDs first via: `gh api graphql -f query='{ repository(owner:"O",name:"R") { pullRequest(number:N) { reviewThreads(first:50) { nodes { id comments(first:1) { nodes { body } } } } } } }'`

## Testing
- Prefer red-green TDD where it makes sense (write a failing test first, then make it pass, then refactor).
- In tests, prefer verbosity over conciseness when it makes the test more readable (e.g. inline data construction instead of helpers).

## Session
- When the user does `/rename`, also run `tmux rename-session "<new name>"` to keep the tmux session name in sync.
- When opening or starting work on a PR, check if the current Claude session name already contains a PR link. If not, append the PR URL to the current session name: run `tmux rename-session "<current name> <pr-url>"` yourself, and print a pasteable `/rename <current name> <pr-url>` for the user to run (Claude cannot invoke `/rename` directly).
- "Current name" means the Claude session name (shown in the session-rename system reminder), not the worktree branch or any auto-generated identifier.
- To find the current session name: read `~/.claude/sessions/*.json`, match on `cwd` matching the current working directory (or parent), and extract the `name` field. Example: `jq -r 'select(.cwd == "<cwd>") | .name' ~/.claude/sessions/*.json`.
