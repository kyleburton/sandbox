#!/usr/bin/env bash

function linuxbrew:install-packages () {
  os-utils:cat-cleanly "$pkg_file" | while read -r line; do
    echo "brew install $line"
    $HOME/.linuxbrew/bin/brew install $line
  done
}

function linuxbrew:install () {
  local pkg_file="${1:-brew.packages}"
  if [ ! -d "$HOME/.linuxbrew" ]; then
    ruby linuxbrew/bin/install.rb
  fi
  
  $HOME/.linuxbrew/bin/brew tap --full github/kyleburton https://github.com/kyleburton/homebrew-kyleburton.git
  $HOME/.linuxbrew/bin/brew update

  linuxbrew:install-packages "$pkg_file"
}
