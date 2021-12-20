#!/usr/bin/env bash

function os-utils:host-is-ubuntu () {
  if ! which lsb_release > /dev/null 2>&1; then
    return 1
  fi

  if [[ $(lsb_release -i -s) == "Ubuntu" ]]; then
    return 0
  fi

  return 1
}

function os-utils:pkg-install-deb () {
  sudo apt-get install -y "$@"
}

function os-utils:pkg-install-yum () {
  sudo apt-get install -y "$@"
}

function os-utils:cat-cleanly () {
  local fname="$1"
  while read -r line; do
    # skip what look like comments
    if [[ $line == \#* ]]; then
      continue
    fi
    # skip blank lines
    if [[ -z $line ]]; then
      continue
    fi
    echo $line
  done < "$fname"
}

