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
- When leaving a review, prefer inline comments anchored to the relevant line over a top-level comment. Only put a finding in the top-level body when it cannot be anchored — it concerns the PR as a whole, or the line it refers to is not part of the diff (GitHub rejects inline comments on unchanged lines).
- Post inline comments as a single review with a `comments` array, not one at a time: `gh api repos/OWNER/REPO/pulls/PR_NUMBER/reviews -X POST --input review.json`, where each entry has `path`, `line`, `side` and `body`. One review means one notification instead of N.
- When fixing inline comments on PRs, reply to the comment explaining what was changed, then resolve the comment thread when appropriate.
- Reply to a review comment via REST: `gh api repos/OWNER/REPO/pulls/PR_NUMBER/comments/COMMENT_ID/replies -X POST -f body="..."`. The PR number is required in the path.
- To resolve threads, use the GraphQL `resolveReviewThread` mutation. The `threadId` must be a `PRRT_...` (review thread ID), NOT a `PRRC_...` (review comment ID). Fetch thread IDs first via: `gh api graphql -f query='{ repository(owner:"O",name:"R") { pullRequest(number:N) { reviewThreads(first:50) { nodes { id comments(first:1) { nodes { body } } } } } } }'`

## Python
- ALWAYS add type annotations when writing or editing Python — every function signature (parameters and return type), including private helpers, tests, and throwaway scripts.
- Use modern builtin generics and unions: `list[str]`, `dict[str, int]`, `X | None`. Never `typing.List`, `typing.Dict`, or `typing.Optional`.
- Import ABCs like `Callable`, `Awaitable`, `Iterator`, and `Sequence` from `collections.abc`, not `typing`.
- Prefer PEP 695 syntax for generics (`def f[T](x: T) -> T:`) over explicit `TypeVar` declarations.

## Testing
- Prefer red-green TDD where it makes sense (write a failing test first, then make it pass, then refactor).
- In tests, prefer verbosity over conciseness when it makes the test more readable (e.g. inline data construction instead of helpers).

## Session
- When the user does `/rename`, also run `tmux rename-session "<new name>"` to keep the tmux session name in sync.
