bake_task ssh_run_remote_script "Run a script remotely."
function ssh_run_remote_script () {
  local user="$1"
  local host="$2"
  local script="$3"
  local rname="$(basename $script)"
  scp $script $user@$host:$rname
  ssh -t $user@$host "bash $rname"
}
