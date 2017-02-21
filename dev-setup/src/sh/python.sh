#!/usr/bin/env bash

PYENV_VERSIONS="2.7.13 3.6.0"

bake_task python:install "use pyenv to install python versions: $PYENV_VERSIONS"
function python:install () {
  for ver in $PYENV_VERSIONS; do
    pyenv install --skip-existing "$ver"
  done
}
