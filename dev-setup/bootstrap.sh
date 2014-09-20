#!/usr/bin/env bash

if !which git; then
  echo "Sorry, you have to have git installed"
  exit 1
fi

test -d software || mkdir software
pushd software
test -d bake || git clone https://github.com/kyleburton/bake.git
popd

echo $(pwd)/software/bake >> .path.dirs

bake install-all
