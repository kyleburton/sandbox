Bash has a feature called [job control](https://www.gnu.org/software/bash/manual/html_node/Job-Control-Basics.html) which allows you to put a program into the background, view your backgrounded jobs, and move a job into the foreground.  Backgrounding of programs can also be used to do run things in parallel.

We can illustrate this by running `vim` and then pressing `CTRL-z`

```bash
> vim a.txt
# then press CTRL-Z

[1]+  Stopped                 vim a.txt
>

# lets create a second one:
> vim b.txt
# then press CTRL-Z

[2]+  Stopped                 vim b.txt
>

# you can see all of your background jobs by running the `jobs` command
> jobs
[1]-  Stopped                 vim a.txt
[2]+  Stopped                 vim b.txt

# we can pull the most recent job to the foreground with `fg`:
> fg
# you are now back in vim b.txt ... pressing CTRL-Z will background it again

[2]+  Stopped                 vim b.txt

> jobs
[1]-  Stopped                 vim a.txt
[2]+  Stopped                 vim b.txt

# you can foreground a specific job with `job %JOBNUM`
> fg %1
# this foregrounds vim a.txt
# use `:wqa` to quit
> fg
# use `:wqa` to quit
> jobs
# none are listed
```

We can use bash's job control to run things in parallel:

```bash
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
```
