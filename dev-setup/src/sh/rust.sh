#!/usr/bin/env bash

bake_task rust:install "Install rust via rustup"
function rust:install () {
  # https://www.rust-lang.org/en-US/install.html
  # curl https://sh.rustup.rs -sSf | sh
  test -f software/install-rustup.sh || curl https://sh.rustup.rs -sSf > software/install-rustup.sh
  bash software/install-rustup.sh
}
