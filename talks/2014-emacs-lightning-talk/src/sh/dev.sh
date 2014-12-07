#!/usr/bin/env bash

bake_task install_cider "Install Emacs/Clojure Cider IDE"
function install_cider () {
  cd software
  if [ ! -d cider ]; then
    git clone git@github.com:clojure-emacs/cider.git
  fi

  cd cider
  git checkout master
  git pull origin master
  git checkout v0.7.0
  cd ..
  cd ..
}

bake_task install_ac_cider "Install Emacs auto completion for Cider"
function install_ac_cider () {
  cd software
  if [ ! -d ac-cider ]; then
    git clone git@github.com:clojure-emacs/ac-cider.git
  fi

  cd ac-cider
  git checkout master
  git pull origin master
  git checkout 0.2.0
  cd ..
  cd ..
}

bake_task install_lein "install leiningen"
function install_lein () {
  if [ ! -e $HOME/bin/lein ]; then
    curl -o $HOME/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
    chmod 755 $HOME/bin/lein
  fi
}

bake_task install_go "install golang"
function install_go () {
  PKG_URL="https://storage.googleapis.com/golang/go1.3.3.linux-amd64.tar.gz"
  PKG_FILE="$(basename $PKG_URL)"
  cd software
  test -f $PKG_FILE || curl -o $PKG_FILE $PKG_URL
  PKG_DIR="$(basename $PKG_FILE)"
  test -d go || tar xzvf $PKG_FILE
  cd ..
  cat <<END > go.env
export GOROOT=$(pwd)/software/go
export GOPATH=$(pwd)/software/go-packages
export PATH="\$PATH:$(pwd)/software/go/bin"
END
}




bake_task install_lein_kinematic_app_template
function install_lein_kinematic_app_template () {
  GIT_URL="https://github.com/kyleburton/sandbox.git"
  pushd software
  test -f sandbox || git clone "$GIT_URL"
  cd sandbox/leiningen/templates/kinematic-app-template
  lein install
  popd
}
