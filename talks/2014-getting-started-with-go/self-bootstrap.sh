set -eu
UNAME="${1:-self}"

if [ ! -e ~/droplet-init/.$UNAME.created ]; then
  (printf "Self\n\n\n\n\ny\ny\n" | adduser --disabled-password --shell /bin/bash $UNAME) || echo OK
  test -d /home/$UNAME/.ssh || mkdir /home/$UNAME/.ssh
  cp ~/.ssh/authorized_keys /home/$UNAME/.ssh/authorized_keys
  chmod 600 /home/$UNAME/.ssh/authorized_keys
  chmod 700 /home/$UNAME/.ssh
  chown -R $UNAME.$UNAME /home/$UNAME/.ssh
  touch ~/droplet-init/.$UNAME.created
fi

if [ ! -e /etc/sudoers.d/99-$UNAME  ]; then
  echo "$UNAME    ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/99-$UNAME
fi
