#!/bin/bash

function next_version {
python - <<END
with open("src/mormot.commit.inc") as file:
  print (int(file.readline().replace("'", "")[4:])+1)
END
}

VERS=$(next_version)
#echo VERS=$VERS

echo -e "'2.0.$VERS'\r">src/mormot.commit.inc
cp src/mormot.commit.inc ~/dev/lib2/src/mormot.commit.inc

git add --all
git commit
git push

echo committed 2.0.$VERS as https://github.com/synopse/mORMot2/commit/`git rev-parse --short HEAD`
