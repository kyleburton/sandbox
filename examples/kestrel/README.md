Kestrel Client Example
======================

Run kestrel in a terminal.  To run this example verbatim, you will need to
create a local.conf:

    $ pwd /Users/kburton/projects/kestrel
    $ cat > local.conf 
    <log>
        filename = "./logs/kestrel.log"
        roll = "daily"
    #    level = "debug"
        level = "info"
    </log>
    
    # where to listen for connections:
    port = 11211
    host = "0.0.0.0"
    
    queue_path = "./logs/kestrel.queue"
    
    timeout = 10


Then create a run.bat:

    $ cat > run.bat
    #!/bin/bash
    VERSION="1.2.1"
    java -jar ./dist/kestrel-$VERSION/kestrel-$VERSION.jar -f local.conf "$@"

Execute Kestrel:

    $ bash run.bat

In another terminal, execute the example client:

    $ sbt run

The client will connect to the single Kestrel server, send in a few messsages
and then pull all of them off of the queue.

    ...
    [info] Running com.github.kyleburton.examples.kestrel.Main 
    Connect to the client...at localhost
    added item to queue[items-queue]: 1:Wed May 05 09:41:23 EDT 2010 res=>'()'
    added item to queue[items-queue]: 2:Wed May 05 09:41:23 EDT 2010 res=>'()'
    added item to queue[items-queue]: 3:Wed May 05 09:41:23 EDT 2010 res=>'()'
    added item to queue[items-queue]: 4:Wed May 05 09:41:23 EDT 2010 res=>'()'
    added item to queue[items-queue]: 5:Wed May 05 09:41:23 EDT 2010 res=>'()'
    added item to queue[items-queue]: 6:Wed May 05 09:41:23 EDT 2010 res=>'()'
    added item to queue[items-queue]: 7:Wed May 05 09:41:23 EDT 2010 res=>'()'
    added item to queue[items-queue]: 8:Wed May 05 09:41:23 EDT 2010 res=>'()'
    added item to queue[items-queue]: 9:Wed May 05 09:41:23 EDT 2010 res=>'()'
    items-queue[1]: '1:Wed May 05 09:41:23 EDT 2010'
    items-queue[2]: '2:Wed May 05 09:41:23 EDT 2010'
    items-queue[3]: '3:Wed May 05 09:41:23 EDT 2010'
    items-queue[4]: '4:Wed May 05 09:41:23 EDT 2010'
    items-queue[5]: '5:Wed May 05 09:41:23 EDT 2010'
    items-queue[6]: '6:Wed May 05 09:41:23 EDT 2010'
    items-queue[7]: '7:Wed May 05 09:41:23 EDT 2010'
    items-queue[8]: '8:Wed May 05 09:41:23 EDT 2010'
    items-queue[9]: '9:Wed May 05 09:41:23 EDT 2010'
    exhausted the queue, got 10 items
    [info] == run ==
    [success] Successful.
    ...


Troubleshooting
===============

    smile: Failed to connect to memcache server localhost:11211: java.net.ConnectException: Connection refused

This means that the configuration for kestrel, or the client may be incorrect
(wrong host or port), or kestrel is not running.


