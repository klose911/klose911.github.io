#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: soreted_iter.py

colors = [ 'red',  'green',  'blue',  'yellow' ]

for color in sorted(colors):
    print (color)

# >>> blue
# green
# red
# yellow

for color in sorted(colors, reverse=True):
    print (color)

# >>> yellow
# red
# green
# blue
# >>> 

def compare_length(c1, c2):
    if len(c1) < len(c2):
        return -1
    if len(c1) > len(c2):
        return 1
    return 0

# print  sorted(colors, cmp=compare_length)

print (sorted(colors, key=len))
# >>> ['red', 'blue', 'green', 'yellow']

