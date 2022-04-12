#!/usr/bin/env bash

function profile:install () {
  if [ ! -d "$HOME/.profile.d" ]; then
    (
    cd "$HOME"
    git clone git@github.com:kyleburton/profile.git .profile.d
    # NB: this should probably be part of the profile install process :/
    for f in .bashrc .dictrc .editrc .gemrc .gitconfig .gitignore .profile .tmux.conf .vimrc; do
      test -f $f && mv $f .dist$f
    done
    )
  fi
}
