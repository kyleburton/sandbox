#!/bin/bash

function this-takes-time () {
    local sleep_time taskname
    # bash has a built in random number generator that you can access with the
    # special variable $RANDOM, you can perform integer arithmetic
    # in bash using double parentheses `echo $(( 2 + 3 ))` will print '5'
    # as bash doesn't support floating point numbers, though the `sleep` command
    # does, this concatenates a digit with a '.' with two more digits to
    # give us a randomized sleep time between 1 and 10 seconds, with two digits
    # of sub-second precision
    sleep_time="$(( (1 + $RANDOM % 5) )).$(( (1 + $RANDOM) % 10 ))$(( (1 + $RANDOM) % 10 ))"
    taskname="${1:-unnamed-task}"
    echo "[$taskname] STARTED $(date) sleep_time=$sleep_time"
    sleep "$sleep_time"
    echo "[$taskname] FINISHED $(date) sleep_time=$sleep_time"
}

# ending a command with `&` runs it, putting it in the background, allowing the script to continue:
echo "[main] starting jobs "
this-takes-time job01 &
this-takes-time job02 &
this-takes-time job03 &
this-takes-time job04 &
this-takes-time job05 &
this-takes-time job06 &
this-takes-time job07 &
this-takes-time job08 &
this-takes-time job09 &
this-takes-time job10 &
echo "[main] all jobs started $(date)"

wait
echo "[main] all jobs completed $(date)"
