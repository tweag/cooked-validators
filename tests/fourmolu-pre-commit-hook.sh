#!/bin/bash
set -e

# command taken from https://github.com/JLLeitschuh/ktlint-gradle  task addKtlintFormatGitPreCommitHook
filesToFormat="$(git --no-pager diff --name-status --no-color --cached | awk '$1 != "D" && $2 ~ /\.hs/ { print $2}')"

echo "files to format $filesToFormat"
for sourceFilePath in $filesToFormat
do
  fourmolu --mode inplace "$(pwd)/$sourceFilePath"
  git add $sourceFilePath
done;
