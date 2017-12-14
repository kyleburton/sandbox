function _docker_is_container_running () {
  local container_name="$1"

  if docker ps --filter name="$container_name" 2>&1 | grep "$container_name"; then
    return 0
  fi

  return 1
}
