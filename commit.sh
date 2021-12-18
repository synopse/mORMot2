#!/bin/bash

git add --all
git commit
git push

echo https://github.com/synopse/mORMot2/commit/`git rev-parse --short HEAD`
