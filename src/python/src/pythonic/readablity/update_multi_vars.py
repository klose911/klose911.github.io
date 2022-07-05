#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: update_multi_vars.py

def fibonacci(n):
    x = 0
    y = 1
    for i in range(n):
        print(x) 
        t = y
        y = x + y
        x = t

print(fibonacci(10))

def fibonacci(n):
    x, y = 0, 1
    for i in range(n):
        print(x)
        x, y = y, x + y

print(fibonacci(10)) 

tmp_x = x + dx * t
tmp_y = y + dy * t
tmp_dx = influence(m, x, y, dx, dy, partial='x')
tmp_dy = influence(m, x, y, dx, dy, partial='y')
x = tmp_x
y = tmp_y
dx = tmp_dx
dy = tmp_dy

x, y, dx, dy = (x + dx * t,
                y + dy * t,
                influence(m, x, y, dx, dy, partial='x'),
                influence(m, x, y, dx, dy, partial='y'))
