# Usage : haproxy [-f <cfgfile>]* [ -vdVD ] [ -n <maxconn> ] [ -N <maxpconn> ]
#         [ -p <pidfile> ] [ -m <max megs> ]
#         -v displays version ; -vv shows known build options.
#         -d enters debug mode ; -db only disables background mode.
#         -V enters verbose mode (disables quiet mode)
#         -D goes daemon
#         -q quiet mode : don't display messages
#         -c check mode : only check config files and exit
#         -n sets the maximum total # of connections (2000)
#         -m limits the usable amount of memory (in MB)
#         -N sets the default, per-proxy maximum # of connections (2000)
#         -p writes pids of all children to this file
#         -sf/-st [pid ]* finishes/terminates old pids. Must be last arguments.
# 
# HA-Proxy version 1.4.15 2011/04/08
# Copyright 2000-2010 Willy Tarreau <w@1wt.eu>
# 

haproxy -V -db -f haproxy-amqp.conf
