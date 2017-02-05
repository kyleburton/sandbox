#!/usr/bin/env bash

set -eu -o pipefail

if [ -n "${DEBUG:-}" ]; then
  set -x
fi

PACKAGES_FILE="packages.ubuntu"
PKG_INSTALLER="pkg-install.ubuntu"

function host-is-ubuntu () {
  if ! which lsb_release > /dev/null 2>&1; then
    return 1
  fi

  if [[ $(lsb_release -i -s) == "Ubuntu" ]]; then
    return 0
  fi

  return 1
}

function pkg-install-deb () {
  sudo apt-get install -y "$@"
}

function pkg-install-yum () {
  sudo apt-get install -y "$@"
}

function init () {
  if host-is-ubuntu; then
    PACKAGES_FILE="packages.ubuntu"
    PKG_INSTALLER="pkg-install-deb"
    return 0
  fi

  echo "ERROR: unable to determine how to install packages on this system: $(uname -a)"
  return 1
}

function all-packages () {
  cat "$PACKAGES_FILE" | grep -v '^#' | grep -v '^$' | tr '\n' ' '
}

function install-packages () {
  local packages="$(all-packages)"
  echo "INSTALLING: $PKG_INSTALLER $packages"
  $PKG_INSTALLER $packages
}

init
install-packages

if ! which ruby ; then
  echo "Sorry, you have to have ruby installed"
  exit 1
fi

ruby linuxbrew/bin/install.rb
echo "Please add the following to your ~/.bash_profile"
echo ""
echo PATH="\$PATH:\$HOME/.linuxbrew/bin"
echo ""
