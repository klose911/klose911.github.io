#!/usr/bin/env python
import pika

connection = pika.BlockingConnection(pika.ConnectionParameters(
    host='localhost'))
channel = connection.channel()

#broadcast all message
channel.exchange_declare(exchange='logs',
                         type='fanout')

#temporarily queue 
#The messages will be lost if no queue is bound to the exchange yet, but it's ok on this case
result = channel.queue_declare(exclusive=True) #after consumes the message, delete the queue 
#random queue name
queue_name = result.method.queue

channel.queue_bind(exchange='logs',
                   queue=queue_name)

print(' [*] Waiting for logs. To exit press CTRL+C')

def callback(ch, method, properties, body):
    print(" [x] %r" % body)

channel.basic_consume(callback,
                      queue=queue_name,
                      no_ack=True)

channel.start_consuming()
