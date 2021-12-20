#!/usr/bin/env bash

function linuxbrew:install-packages () {
  os-utils:cat-cleanly "$pkg_file" | while read -r line; do
    local pkg_name="$(echo "$line" | cut -f1 -d ' ')"
    if brew ls --versions "$pkg_name"; then
      bake_echo_green "OK: brew $pkg_name already installed ($line)"
      continue
    fi
    bake_echo_blue "brew install $pkg_name via $line"
    $HOME/.linuxbrew/bin/brew install $line || echo "Mayb ok if it's already installed? $line"
  done
}

bake_task linuxbrew:install "Install packages via brew"
function linuxbrew:install () {
  local pkg_file="${1:-brew.packages}"
  if [ ! -d "$HOME/.linuxbrew" ]; then
    ruby linuxbrew/bin/install.rb
  fi
  
  $HOME/.linuxbrew/bin/brew tap --full github/kyleburton https://github.com/kyleburton/homebrew-kyleburton.git
  $HOME/.linuxbrew/bin/brew update

  linuxbrew:install-packages "$pkg_file"
}
