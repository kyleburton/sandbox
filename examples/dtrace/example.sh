#!/usr/bin/env bash

# References
# * http://www.oracle.com/technetwork/server-storage/solaris/dtrace-tutorial-142317.html

# dtrace is intended for use in production systems, when it's not in use there
# is zero overhead as soon as you introduce a probe, There is a penalty, dtrace
# is designed to minimize that impact.  the cost is proportional to the
# quesiton you ask.


# list all the probes in the system
# sudo dtrace -l
# ... about 283k probes on my mac


# sudo dtrace -n <provider> <module> <function> <probe-name>
# sudo dtrace -n syscall:::

# sudo dtrace -n syscall:::'{ trace(execname); }'

# aggregations (WOW!)
# echo "make sure to CTRL+C after a bit..."
# sudo dtrace -n syscall:::entry'{ @[execname] = count(); }'
# this produces a histogram

# mac osx 'sip' or 'siff' and dtrace


# jamfAgent
# sudo dtrace -n syscall:::entry'/execname == "jamfAgent"/{ @[probefunc] = count(); }'

# WOW!
# sudo dtrace -n syscall:::entry'/execname == "jamfAgent"/{ @[ustack()] = count(); }'

# user level process tracing
# sudo dtrace -n 'pid20849::malloc:entry'
chrome_pid="$(ps wwwaux | grep [C]hrome | head -n1 | awk '{print $2}')"
sudo dtrace -n "pid$chrome_pid::malloc:entry{ @[\"malloc\"] = quantize(arg2); }"
# thats ... impressive

# cd /var/tmp/
# this measures how long it takes to do writes on the system, nanosecond
# latency for how long writes take...
# cat <<END
# #!/usr/sbin/trace -s
# syscall::write:entry
# {
#   # ts is made up
#   self->ts = timestamp;
# }
# syscall::write:return
# {
#   @ = quantize(timestammp - self->ts);
#   self->ts = 0;
# }
# END
# 
# 

# nfsv3:::op-read-start,
# nfsv3:::op-write-start
# {
#     self->ts = timestamp;
# }

# nfsv3:::op-read-done,
# nfsv3:::op-write-done
# {
#   @[probename = "op-write-done" ? "write" : "read" ] = quantize(timestamp - self->ts);)
#    ... he added more ...
#   self->ts = 0;
#   self->io = 0;
#   self->sync = 0;
# }


# basic performance goals:
# * get idel out of the system
#   * figure out why work isn't getting done
# * get idle into the system
#   * don't waste cycles
#   * be more efficient

# sched:::off-cpu
# /self-.ts/
# {
#   self->off = timestamp;
# }
# ...


# what does DTrace look like on JVM programs?
# what does DTrace look like on golang programs?

