#!/usr/bin/env bash

bake_task erlang-install-rebar "Install Basho's rebar build tool"
function erlang-install-rebar () {
  pushd $(bake_bakefile_dir)/software >/dev/null
  test -d rebar || git clone https://github.com/basho/rebar.git
  cd rebar
  test -x ./rebar || ./bootstrap
  popd >/dev/null
  add_path $(bake_bakefile_dir)/software/rebar
}

bake_task erlang-install-all "Install all the Erlang goodies"
function erlang-install-all () {
  erlang-install-rebar
}


