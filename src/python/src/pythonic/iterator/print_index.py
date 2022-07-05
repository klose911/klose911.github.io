#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: print_index.py

colors = [ 'red',  'green',  'blue',  'yellow' ]

for i in range(len(colors)):
    print (i, '->', colors[i])

# >>> 0 -> red
# 1 -> green
# 2 -> blue
# 3 -> yellow
# >>>

for i, color in enumerate(colors):
    print (i,  '->', color)
    
