#!/usr/bin/env bash
set -eu

LEIN_URL="https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"
CIDER_GIT_URL="https://github.com/clojure-emacs/cider.git"
AC_CIDER_GIT_URL="https://github.com/clojure-emacs/ac-cider.git"
PROJECT_ROOT="$(pwd)"

STDOUT_IS_TERMINAL=""
COLOR_NORMAL=""
COLOR_RED=""
COLOR_LRED=""
COLOR_BLUE=""
COLOR_LBLUE=""
COLOR_GREEN=""
COLOR_LGREEN=""

function init () {
  STDOUT_IS_TERMINAL="yes"
  COLOR_NORMAL="\e[00m"
  COLOR_RED="\e[00;31m"
  COLOR_LRED="\e[01;31m"
  COLOR_BLUE="\e[00;34m"
  COLOR_LBLUE="\e[01;34m"
  COLOR_GREEN="\e[00;32m"
  COLOR_LGREEN="\e[01;32m"
}

function echo_red () {
  local opts=""
  while [[ "$1" == -* ]]; do
    opts="$opts $1"
    shift
  done
  echo $opts -e "${COLOR_RED}$@${COLOR_NORMAL}"
}

function echo_lred () {
  local opts=""
  while [[ "$1" == -* ]]; do
    opts="$opts $1"
    shift
  done
  echo $opts -e "${COLOR_LRED}$@${COLOR_NORMAL}"
}

function echo_blue () {
  local opts=""
  while [[ "$1" == -* ]]; do
    opts="$opts $1"
    shift
  done
  echo $opts -e "${COLOR_BLUE}$@${COLOR_NORMAL}"
}

function echo_lblue () {
  local opts=""
  while [[ "$1" == -* ]]; do
    opts="$opts $1"
    shift
  done
  echo $opts -e "${COLOR_LBLUE}$@${COLOR_NORMAL}"
}

function echo_green () {
  local opts=""
  while [[ "$1" == -* ]]; do
    opts="$opts $1"
    shift
  done
  echo $opts -e "${COLOR_GREEN}$@${COLOR_NORMAL}"
}

function echo_lgreen () {
  local opts=""
  while [[ "$1" == -* ]]; do
    opts="$opts $1"
    shift
  done
  echo $opts -e "${COLOR_LGREEN}$@${COLOR_NORMAL}"
}


function has_binary_on_path () {
  local bname="$1"
  which "$bname" > /dev/null 2>&1
}

function save_url_to_file () {
  local url="$1"
  local fname="$2"

  if has_binary_on_path curlx; then
    echo_lgreen "...curl $ull to $fname"
    curl -s "$url" > "$fname"
    return 0
  fi

  if has_binary_on_path wget; then
    echo_lgreen "...wget $url to $fname"
    wget --quiet -O "$fname" "$url"
    return 0
  fi

  echo "ERROR: can't find curl or wget, unable to download files"
  exit 1
}

function install_lein () {
  test -d bin || mkdir bin

  echo_green "leiningen: install"
  if [ ! -f bin/lein ]; then
    save_url_to_file "$LEIN_URL" "bin/lein"
  fi

  if [ ! -x bin/lein ]; then
    chmod 755 bin/lein
  fi

  echo_green "leiningen: init"
  ./bin/lein version 2>&1 | sed 's/^/lein: /'
}

function install_cider () {
  echo_red "install cider"
}

function install_ac_cider () {
  echo_red "install ac cider"
}

function make_emacs_runner () {
  echo_red "make emacs runner"
}


function install_emacs () {
  if ! has_binary_on_path emacsx; then
    local distname="$(lsb_release -i | cut -f2 -d: | sed 's/ //g')"
    echo_green "installing emacs...$distname"
    case "$distname" in
      Ubuntu)
        sudo apt-get install emacs
        ;;
      *)
        echo_red "ERROR: don't know how to install emacs on $distname"
        exit 1
    esac
  fi
}

function uninstall () {
  rm -rf ./bin
}

function install () {
  install_emacs
  install_lein
  install_cider
  install_ac_cider
  make_emacs_runner
}

function show_help () {
  echo "install-linux.sh <cmd> <args...>"
  echo ""
  echo "  help            show this help"
  echo "  install         install all dependencies"
  echo "  uninstall       uninstall software"
  echo ""
}

function main () {
  local cmd="${1:-help}"
  case "$cmd" in
    install)
      init
      install
      ;;
    uninstall)
      init
      uninstall
      ;;
    help)
      show_help
      ;;
    *)
      show_help
      ;;
  esac
}

main "$@"
