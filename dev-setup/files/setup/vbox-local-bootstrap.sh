#!/usr/bin/env bash
set -eEuo pipefail
set -x


function apt-install {
  for pkg in $@; do
    if ! apt list --installed | grep "^$pkg"; then
      # https://askubuntu.com/questions/17823/how-to-list-all-installed-packages
      # aptitude search '~i!~M'
      sudo apt install "$pkg"
    fi
  done
}


case "${1:-}" in
  part2)
    if [[ ! -f "$HOME/code/github.com/kyleburton/sandbox" ]]; then
      GIT_SSH_COMMAND="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no" \
        git clone \
        git@github.com:kyleburton/sandbox.git \
        "$HOME/code/github.com/kyleburton/sandbox"
    fi
    ;;
  *)
    test -f /usr/share/virtualbox/VBoxGuestAdditions.iso \
      || sudo apt-get install build-essential dkms virtualbox-guest-additions-iso
    
    echo "TODO: install homebrew"
    
    if [[ ! -f $HOME/.linuxbrew/bin/brew  ]]; then 
      bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    
    test -d "$HOME/code/github.com/kyleburton" || mkdir -p "$HOME/code/github.com/kyleburton"
    
    apt-install git aptitude rsync openssh-server
  ;;
esac



