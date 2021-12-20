#!/usr/bin/env bash

test -d software/bin || mkdir -p software/bin
if [ ! -e software/bin/bake ]; then
  curl https://raw.githubusercontent.com/kyleburton/bake/master/bake > software/bin/bake
  chmod 755 software/bin/bake
fi

export PATH="$PATH:software/bin/bake"
bake install
