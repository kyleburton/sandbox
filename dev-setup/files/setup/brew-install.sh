#!/usr/bin/env bash
set -eEuo pipefail


test -f ./files/setup/brew-installer.sh || curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh > ./files/setup/homebrew-installer.sh

if [[ ! -d /home/linuxbrew/.linuxbrew ]]; then
  /bin/bash -c "./files/setup/homebrew-installer.sh"
fi

