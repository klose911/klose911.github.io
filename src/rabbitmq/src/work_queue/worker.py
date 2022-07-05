#!/usr/bin/env python
import pika
import time

#like send.py
connection = pika.BlockingConnection(pika.ConnectionParameters(
    host='localhost'))
channel = connection.channel()
channel.queue_declare(queue='work_queue', durable=True)


def callback(ch, method, properties, body):
    print " [x] Received %r" % (body,)
    time.sleep( body.count('.') )
    print " [x] Done"
    ch.basic_ack(delivery_tag = method.delivery_tag)

channel.basic_qos(prefetch_count=1)
channel.basic_consume(callback,
                      queue='work_queue')

print(' [*] Waiting for messages. To exit press CTRL+C')
# enter a never-ending loop that waits for data and runs callbacks whenever necessary
channel.start_consuming()
