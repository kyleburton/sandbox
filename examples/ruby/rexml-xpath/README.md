REXML Example
===========

This is a command line utility you can use to execute arbitrary xpath expressions against xml documents.

    $ ./xpath $HOME/Library/VirtualBox/Machines/rn-ubuntu/rn-ubuntu.xml "/VirtualBox/Machine/Hardware/GuestProperties/GuestProperty[@name='/VirtualBox/GuestInfo/Net/0/V4/IP']/@value" 
    192.168.37.85
    $
