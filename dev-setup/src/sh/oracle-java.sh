#!/usr/bin/env bash

# NB: this is Ubunntu / Debian specific
bake_task oracle-java:install "Install Oracle JDK"
function oracle-java:install () {
  # silent:  http://askubuntu.com/questions/190582/installing-java-automatically-with-silent-option
  echo debconf shared/accepted-oracle-license-v1-1 select true | sudo debconf-set-selections
  echo debconf shared/accepted-oracle-license-v1-1 seen true   | sudo debconf-set-selections

  # java: http://tipsonubuntu.com/2016/07/31/install-oracle-java-8-9-ubuntu-16-04-linux-mint-18/
  set -x
  sudo add-apt-repository -y ppa:webupd8team/java
  sudo apt update
  sudo apt install -y oracle-java8-installer
  sudo apt install -y oracle-java8-set-default
  set +x
}

