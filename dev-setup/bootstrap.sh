#!/usr/bin/env bash

set -eu -o pipefail

source src/sh/fake-bake.sh
source src/sh/python.sh
source src/sh/oracle-java.sh
source src/sh/leiningen.sh
source src/sh/os-utils.sh
source src/sh/profile.sh
source src/sh/linuxbrew.sh

PACKAGES_FILE="packages.ubuntu"
PKG_INSTALLER="os-utils:pkg-install.ubuntu"


if [ -n "${DEBUG:-}" ]; then
  set -x
fi

function init () {
  if os-utils:host-is-ubuntu; then
    PACKAGES_FILE="packages.ubuntu"
    PKG_INSTALLER="os-utils:pkg-install-deb"
    return 0
  fi

  echo "ERROR: unable to determine how to install packages on this system: $(uname -a)"
  return 1
}

function all-packages () {
  cat "$PACKAGES_FILE" | grep -v '^#' | grep -v '^$' | tr '\n' ' '
}

function install-os-packages () {
  local packages="$(all-packages)"
  echo "INSTALLING: $PKG_INSTALLER $packages"
  $PKG_INSTALLER $packages
}


init
install-os-packages

if ! which ruby ; then
  echo "Sorry, you have to have ruby installed"
  exit 1
fi

linuxbrew:install brew.packages
profile:install
python:install
oracle-java:install
lein:init

echo "Please add the following to your ~/.bash_profile"
echo ""
echo PATH="\$PATH:\$HOME/.linuxbrew/bin"
echo ""
