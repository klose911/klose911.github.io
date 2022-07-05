#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: unpack.py

p =  'Raymond', 'Hettinger',  0x30, 'python@example.com'

#('Raymond', 'Hettinger', 48, 'python@example.com')

# 其它语言的常用方法/习惯 
fname = p[0]
lname = p[1]
age = p[2]
email = p[3]

fname, lname, age, email = p
print(fname)
print(lname)
print(age)
print(email) 
