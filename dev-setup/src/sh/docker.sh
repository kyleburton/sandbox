#!/usr/bin/env bash

bake_task docker:install "Install docker!"
function docker:install () {
   sudo usermod -a -G docker "$LOGNAME"
}
