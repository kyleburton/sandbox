#!/usr/bin/env bash
set -eEuo pipefail
set -x

set -x
test -f /usr/share/virtualbox/VBoxGuestAdditions.iso \
  || sudo apt-get install build-essential dkms virtualbox-guest-additions-iso

echo "TODO: install homebrew"

if [[ ! -f $HOME/.linuxbrew/bin/brew  ]]; then 
  bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

test -d "$HOME/code/github.com/kyleburton" || mkdir -p "$HOME/code/github.com/kyleburton"

if ! apt list --installed | grep ^git; then
  sudo apt install git
fi

if [[ -f "$HOME/code/github.com/kyleburton/sandbox" ]]; then
  GIT_SSH_COMMAND="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no" \
    git clone \
    git@github.com:kyleburton/sandbox.git \
    "$HOME/code/github.com/kyleburton/sandbox"
fi

