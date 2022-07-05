#!/usr/bin/env python
import sys
import pika

# connect to local machine
connection = pika.BlockingConnection(pika.ConnectionParameters(
    'localhost'))
channel = connection.channel()
channel.queue_declare(queue='work_queue', durable=True)

message = ' '.join(sys.argv[1:]) or "Hello World!" 
channel.basic_publish(exchange='', 
                      routing_key='work_queue', # queue name 
                      body=message,
                      properties=pika.BasicProperties(
                          delivery_mode = 2, #make message persistent
                      ))
print(" [x] Sent %r" % message)
connection.close()

