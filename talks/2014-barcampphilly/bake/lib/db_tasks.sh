bake_task db_shell
function db_shell () {
  sudo -u postgres psql aninbox_development
}

bake_task db_exec_file
function db_exec_file () {
  local fname="$1"
  sudo -u postgres psql aninbox_development -f $fname
}
