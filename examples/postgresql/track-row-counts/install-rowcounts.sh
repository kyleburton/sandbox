set -exu
DBNAME="$1"
sudo -u postgres psql $DBNAME -f rowcounts.sql

