set -x
export RABBITMQ_NODE_IP_ADDRESS=127.0.0.1
export RABBITMQ_NODE_PORT=15672
export RABBITMQ_NODENAME=rabbit02
export RABBITMQ_MNESIA_BASE=$HOME/tmp/harabbit/rabbit01/mnesia
export RABBITMQ_LOG_BASE=$HOME/tmp/harabbit/rabbit01/logs

for dir in $RABBITMQ_MNESIA_BASE $RABBITMQ_LOG_BASE; do
  test -d $dir || mkdir -p $dir
done

rabbitmq-server
