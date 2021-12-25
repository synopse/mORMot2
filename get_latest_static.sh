#!/bin/bash

# -------------------------------------------------------------------------------
# DEPRECATED: simply download latest https://synopse.info/files/mormot2static.7z
# -------------------------------------------------------------------------------

# Update a static folder content from the latest [pre]release of mORMot2
# Required tools: jq curl wget 7zip. On Ubuntu can be installed by
# sudo apt install curl wget jq p7zip-full

echo "Getting the latest pre-release URL..."
LATEST_URL=$(curl --silent https://api.github.com/repos/synopse/mORMot2/releases | jq -r '.[0].assets[0].browser_download_url')

echo "Downloading from $LATEST_URL ..."
DOWNLOAD_PATH=/tmp/mormot_static_latest.7z
wget -q -O $DOWNLOAD_PATH "$LATEST_URL"

echo "Unpacking to ./static ..."
rm -Rf ./static
7za x $DOWNLOAD_PATH -o./