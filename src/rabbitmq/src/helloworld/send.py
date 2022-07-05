#!/usr/bin/env python
import pika

# connect to local machine
connection = pika.BlockingConnection(pika.ConnectionParameters(
    'localhost'))
channel = connection.channel()
# before sending we need to make sure the recipient queue exists. If we send a message to non-existing location, RabbitMQ will just trash the message
# create a queue which named 'hello' 
channel.queue_declare(queue='hello')
# a message can never be sent directly to the queue, always needs to go through an exchange
channel.basic_publish(exchange='', #a default exchange identified by an empty string. This exchange it allows us to specify exactly to which queue the message should go
                      routing_key='hello', # queue name 
                      body='Hello World!') # message content 
print(" [x] Sent 'Hello World!'")
connection.close()

