#!/bin/bash

VERS=`cat src/mormot.commit.inc|sed 's/.*2.0.//; s/''*//' `
#echo VERS=$VERS
V2=${VERS::-2}
#echo V2=$V2
V2=$((V2+1))
echo -e "'2.0.$V2'\r">src/mormot.commit.inc
cp src/mormot.commit.inc ~/dev/lib2/src/mormot.commit.inc

git add --all
git commit
git push

echo committed 2.0.$V2 as https://github.com/synopse/mORMot2/commit/`git rev-parse --short HEAD`
