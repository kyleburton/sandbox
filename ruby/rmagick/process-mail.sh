#!/bin/sh

ORGMAIL=/var/mail/$LOGNAME
LFILE=$HOME/process-mail.log

if cd $HOME/personal/projects/sandbox/ruby/rmagick && test -s $ORGMAIL && lockfile -r0 -l1024 .newmail.lock 2>/dev/null; then
   trap "rm -f .newmail.lock" 1 2 3 13 15
   echo "$(date) obtained lock" >> $LFILE
   umask 077
   lockfile -l1024 -ml
   # honestly this looks like a race-condition...
   cat $ORGMAIL >>.newmail && cat /dev/null >$ORGMAIL
   lockfile -mu
   # this formail command sends each email from the mail file out
   # to an exteranl process, in this case procmail it then
   # removes the temporary file
   # formail -s procmail <.newmail && rm -f .newmail
   ruby ./rmail.rb .newmail >> $LFILE
   rm -f .newmail
   rm -f .newmail.lock
   echo "$(date) processing completed" >> $LFILE
else
   echo "$(date) Empty mbox: $ORGMAIL" >> $LFILE
fi
exit 0
