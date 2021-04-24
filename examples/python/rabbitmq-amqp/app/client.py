import pika
import sys
import itertools

amqp_hostname = "localhost"
amqp_exchange = ""
amqp_queue_name = "qn-hello"
amqp_routing_key = "rk-hello"


def send(msg="Hello World!", count="1"):
    count = int(count)
    connection = pika.BlockingConnection(pika.ConnectionParameters(amqp_hostname))
    channel = connection.channel()
    channel.queue_declare(queue=amqp_queue_name)
    for ii in range(count):
        print(
            r" [{}] publishing x={}; r={}; qn={}; msg={}".format(
                ii, amqp_exchange, amqp_routing_key, amqp_queue_name, msg
            )
        )
        channel.basic_publish(
            exchange=amqp_exchange, routing_key=amqp_routing_key, body=msg
        )
    connection.close()


def recv(ch, method, properties, body):
    print(
        r"recv: ch={}; method={}; properties={}; body={}".format(
            ch, method, properties, body
        )
    )


def subscribe():
    connection = pika.BlockingConnection(pika.ConnectionParameters(amqp_hostname))
    channel = connection.channel()
    channel.queue_declare(queue=amqp_queue_name)
    channel.basic_consume(
        queue=amqp_queue_name, auto_ack=True, on_message_callback=amqp_recv
    )
    connection.close()


def print_usage():
    print(r"{}: send|subscribe".format(sys.argv[0]))
    print(r"  send topic message [repeat-count=1]".format())
    print(r"  recv topic".format())


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print_usage()
        sys.exit(1)

    for arg in sys.argv[1:]:
        print(r"arg={}".format(arg))
        if "send" == arg:
            send(*sys.argv[2:])
