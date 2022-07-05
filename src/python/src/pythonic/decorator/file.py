#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: file.py

f = open('data.txt')
try:
    data = f.read()
finally:
    f.close()

with open('data.txt') as f:
    data = f.read()
