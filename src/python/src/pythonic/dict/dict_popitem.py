#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: dict_popitem.py

d = {'matthew': 'blue', 'rachel': 'green', 'raymond': 'red'}

while d:
    key, value = d.popitem()
    print(key, '->', value)

# raymond -> red
# rachel -> green
# matthew -> blue

print(d)
# {} 
