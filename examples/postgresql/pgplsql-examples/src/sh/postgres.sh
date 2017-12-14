#!/usr/bin/env bash

DOCKER_POSTGRES_IMAGE_TAG="${DOCKER_POSTGRES_IMAGE_TAG:-postgres:10}"
DOCKER_POSTGRES_CONTAINER_NAME="${DOCKER_POSTGRES_CONTAINER_NAME:-plpgsql-examples}"
POSTGRES_PASSWORD="${POSTGRES_PASSWORD:-theweakestpassword}"
POSTGRES_USERNAME="${POSTGRES_USERNAME:-postgres}"

# TODO: allow the port to be exposed to the local machine (default 5432)
# TODO: move this into a set of recipies in a git repo
# TODO: implement utilities to maintain the ~/.pgpass file
# TODO: allow the data directory to be mounted
# TODO: allow the port and ip binding to be specified as env vars w/defaults

function postgres:init () {
  docker pull "$DOCKER_POSTGRES_IMAGE_TAG"
}

bake_task postgres "Control the postgres docker container"
function postgres () {
  local cmd="${1:-}"

  case "$cmd" in 
    start)
      docker run \
        --rm \
        -d \
        -e "POSTGRES_PASSWORD=$POSTGRES_PASSWORD" \
        --name "$DOCKER_POSTGRES_CONTAINER_NAME" \
        "$DOCKER_POSTGRES_IMAGE_TAG"
      ;;
    stop)
      docker stop "$DOCKER_POSTGRES_CONTAINER_NAME"
      ;;
    status)
      _docker_is_container_running "$DOCKER_POSTGRES_CONTAINER_NAME"
      ;;
    psql)
      set -x
      shift
      docker exec -ti "$DOCKER_POSTGRES_CONTAINER_NAME" /usr/bin/psql -U "$POSTGRES_USERNAME" "$@"
      ;;
    *|help|h|--help|-h)
      bake_echo_red ""
      bake_echo_red "Error: you must supply an action:"
      bake_echo_red ""
      for action in stop start status psql; do
        bake_echo_red "  bake postgres psql $action"
      done
      bake_echo_red ""
      ;;
  esac
}
