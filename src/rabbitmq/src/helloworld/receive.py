#!/usr/bin/env python
import pika

#like send.py
connection = pika.BlockingConnection(pika.ConnectionParameters(
    host='localhost'))
channel = connection.channel()
channel.queue_declare(queue='hello')

def callback(ch, method, properties, body):
    print(" [x] Received %r" % body)

# subscribing a callback function to a queue 
channel.basic_consume(callback,
                      queue='hello',
                      no_ack=True) # turned message acknowledgments off 

print(' [*] Waiting for messages. To exit press CTRL+C')
# enter a never-ending loop that waits for data and runs callbacks whenever necessary
channel.start_consuming()
