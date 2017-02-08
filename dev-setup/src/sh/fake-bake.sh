#!/usr/bin/env bash

# these functions allow the libraries bootstrap.sh uses to both be used from
# bootstrap.sh (just a shell script) and from bake, which avoids the
# pre-requisite issue of having bake already installed and us wanting to install
# it via brew


function bake_task () {
  return 0 # no op
}

function bake_echo_red () {
  return 0 # no op
}

function bake_echo_green () {
  return 0 # no op
}

function bake_echo_blue () {
  return 0 # no op
}

function bake_echo_yellow () {
  return 0 # no op
}
