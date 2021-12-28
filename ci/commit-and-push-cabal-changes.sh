#! /bin/bash
changes=$(git status --porcelain | grep "^ M" | grep ".cabal" | sed 's/ M //')
if [[ ! -z "$changes" ]]; then
  git config user.name github-actions
  git config user.email github-actions@github.com
  git add $changes
  git commit -m "Generated cabal files"
  git push
fi
