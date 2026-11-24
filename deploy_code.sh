#!/usr/bin/env bash
# Deploy the code to episerv: check that everything local is on GitHub, then
# pull it on the server. Stops without touching the server if anything is
# uncommitted, untracked or not pushed.
#
# Usage: ./deploy_code.sh [flags]
#   --check-only         only check that everything is on GitHub, do not pull on episerv

set -euo pipefail

# run from the repo root whatever the current directory
cd "$(dirname "$0")"

check_only=false
for arg in "$@"; do
  case "$arg" in
    -h | --help)
      sed -n '2,7p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    --check-only) check_only=true ;;
    *)
      echo "Unknown flag: $arg (see ./deploy_code.sh --help)" >&2
      exit 1
      ;;
  esac
done

echo "==> Checking that everything is on GitHub"

if [ -n "$(git status --porcelain)" ]; then
  echo "Uncommitted or untracked changes, commit and push them first:" >&2
  git status --short >&2
  exit 1
fi

branch="$(git rev-parse --abbrev-ref HEAD)"
git fetch --quiet origin

if ! git rev-parse --verify --quiet "origin/$branch" >/dev/null; then
  echo "Branch '$branch' is not on GitHub, push it first: git push -u origin $branch" >&2
  exit 1
fi

ahead="$(git rev-list --count "origin/$branch..HEAD")"
behind="$(git rev-list --count "HEAD..origin/$branch")"
if [ "$ahead" -gt 0 ]; then
  echo "$ahead commit(s) not pushed to GitHub, run: git push" >&2
  git log --oneline "origin/$branch..HEAD" >&2
  exit 1
fi
if [ "$behind" -gt 0 ]; then
  echo "Note: GitHub has $behind commit(s) you don't have locally; the server will get them too."
fi

echo "OK: '$branch' is up to date on GitHub ($(git rev-parse --short HEAD))"

if $check_only; then
  echo "==> Done (--check-only, episerv not touched)"
  exit 0
fi

echo "==> Pulling the code on episerv"
ssh episerv "cd EVD-COD17-butembo && git pull"

echo "==> Done"
