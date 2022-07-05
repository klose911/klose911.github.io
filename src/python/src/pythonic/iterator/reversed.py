#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: reversed.py

colors = [ 'red',  'green',  'blue',  'yellow' ]

for i in range(len(colors) - 1,  -1,  -1):
    print (colors[i])

# >>> yellow
# blue
# green
# red

for  color in reversed(colors):
    print (color) 
