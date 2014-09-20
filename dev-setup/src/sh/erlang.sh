#!/usr/bin/env bash

bake_task erlang-install-erlang "Install erlang"
function erlang-install-erlang () {
  local SDIR=$(bake_bakefile_dir)/software
  pushd $SDIR >/dev/null
  local OTP_URL=http://www.erlang.org/download/otp_src_17.3.tar.gz
  local OTP_FNAME=$(basename $OTP_URL)
  test -f $OTP_FNAME || curl -O $OTP_URL
  local OTP_DNAME=$(basename $OTP_FNAME .tar.gz)
  test -d $OTP_DNAME || tar xzvf $OTP_FNAME
  cd $OTP_DNAME
  local VER=$(echo $OTP_DNAME | tr _ \\n | tail -n +3)
  test -f Makefile || ./configure --prefix=$SDIR/erlang_$VER
  test -f ./bin/erl || make
  make install
  popd >/dev/null
}

bake_task erlang-install-rebar "Install Basho's rebar build tool"
function erlang-install-rebar () {
  pushd $(bake_bakefile_dir)/software >/dev/null
  test -d rebar || git clone https://github.com/rebar/rebar.git
  cd rebar
  test -x ./rebar || ./bootstrap
  popd >/dev/null
  add_path $(bake_bakefile_dir)/software/rebar
}

bake_task erlang-install-all "Install all the Erlang goodies"
function erlang-install-all () {
  erlang-install-erlang
  erlang-install-rebar
}


