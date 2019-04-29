#!/usr/bin/env bash
set -eEu -o pipefail

[[ -n "${DEBUG:-}" ]] && set -x

function cmd.show-help () {
  echo "$0 help ..."
  echo ""
  awk '/[s]tart-main-commands/{flag=1;next}/end-main-commands/{flag=0}flag' "$0" \
    | grep -E '[-_.a-zA-Z0-9]+)' \
    | cut -f1 -d')' \
    | while IFS= read -r cmd; do
    echo "  $0 $cmd ..."
  done
  echo ""
}


function cmd.postgres-start () {
  exec docker-compose up -d postgres
}


function cmd.psql () {
  docker-compose exec postgres psql -U postgres etest
}

function main () {
  local cmd="${1:-}"

  case "$cmd" in # start-main-commands
    start)
      shift
      cmd.postgres-start "$@"
      ;;

    help)
      shift
      cmd.show-help "$@"
      ;;

    psql)
      shift
      cmd.psql "$@"
      ;;

    *)
      cmd.psql "$@"
      ;;
  esac # end-main-commands
}

main "$@"
