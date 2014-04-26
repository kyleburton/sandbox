set -eu

if [ ! -e ~/droplet-init/.self.created ]; then
  (printf "Self\n\n\n\n\ny\ny\n" | adduser --disabled-password --shell /bin/bash self) || echo OK
  test -d /home/self/.ssh || mkdir /home/self/.ssh
  cp ~/.ssh/authorized_keys /home/self/.ssh/authorized_keys
  chmod 600 /home/self/.ssh/authorized_keys
  chmod 700 /home/self/.ssh
  chown -R self.self /home/self/.ssh
  touch ~/droplet-init/.self.created
fi

cp ~/droplet-init/files/etc/sudoers.d/99-self /etc/sudoers.d/
