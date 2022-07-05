#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: dict_zip.py

names = ['raymond', 'rachel', 'matthew']
colors = ['red', 'green', 'blue']
# d = dict(izip(names, colors))

d = dict(zip(names, colors)) 
print (d) 

# >>> {'raymond': 'red', 'rachel': 'green', 'matthew': 'blue'}
