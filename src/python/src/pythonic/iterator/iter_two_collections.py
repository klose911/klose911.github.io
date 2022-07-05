#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: iter_two_collections.py


names = ['raymond', 'rachel', 'matthew']
colors = ['red', 'green', 'blue', 'yellow']

n = min(len(names), len(colors))

for i in range(n):
    print (names[i], '->', colors[i])

# >>> raymond -> red
# rachel -> green
# matthew -> blue

for name, color in zip(names, colors):
    print (name, '->', color)
