set -eu

DROPLET_NAME=golang-1
REGION=nyc2
DROPLET_SIZE=1gb
IMAGE_NAME=ubuntu-13-10-x64
SSH_KEY_ID=$(diocean ssh-keys ls  | cut -f1 | head -n 2 | tail -n 1)
PRIVATE_NETWORKING=false
BACKUPS_ENABLED=false

droplet_exists () {
  diocean droplets ls | grep -q "	$1	"
}

droplet_ip_address () {
  diocean droplets ls | grep "	$1	" | cut -f 7
}

if droplet_exists $DROPLET_NAME; then
  echo "Droplet already exists: $DROPLET_NAME"
else
  echo "creating: $DROPLET_NAME"
  diocean -w droplets new $DROPLET_NAME $DROPLET_SIZE $IMAGE_NAME $REGION $SSH_KEY_ID $PRIVATE_NETWORKING $BACKUPS_ENABLED
fi


DROPLET_IP_ADDRESS=$(droplet_ip_address $DROPLET_NAME)
# diocean ssh fix-known-hosts 

CMD="${1:-all}"
case "$CMD" in
  bootstrap-root)
    rsync -avz . root@$DROPLET_IP_ADDRESS:droplet-init/
    ssh root@$DROPLET_IP_ADDRESS "cd droplet-init; bash self-bootstrap.sh"
    ;;
  bootstrap-self)
    rsync -avz . self@$DROPLET_IP_ADDRESS:droplet-init/
    ssh self@$DROPLET_IP_ADDRESS "cd droplet-init; bash local-bootstrap.sh"
    echo "RUN: ssh self@$DROPLET_IP_ADDRESS"
    ;;
  reset)
    ssh root@$DROPLET_IP_ADDRESS "userdel self; rm -rf /home/self; rm -rf droplet-init"
    ;;
  all)
    bash $0 bootstrap-root
    bash $0 bootstrap-self
    ;;
  *)
    echo "please specify a command"
esac

