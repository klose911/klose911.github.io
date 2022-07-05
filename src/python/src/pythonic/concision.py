#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: concision.py

result = []

for i in range(10):
    s = i ** 2
    result.append(s)

print (sum(result))

print (sum(i**2 for i in range(10))) 
