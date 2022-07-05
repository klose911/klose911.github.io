#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: dict_count.py

colors = ['red', 'green', 'red', 'blue', 'green', 'red']

# 简单，基本的计数方法。适合初学者起步时学习。
d = {}
for color in colors:
    if color not in d:
        d[color] = 0
    d[color] += 1

# >>>{'red': 3, 'green': 2, 'blue': 1}

d = {}

for color in colors:
    d[color] = d.get(color, 0) + 1
# >>> {'red': 3, 'green': 2, 'blue': 1}

# 稍微潮点的方法，但有些坑需要注意，适合熟练的老手。
from collections import defaultdict

d = defaultdict(int)
# >>> defaultdict(<class 'int'>, {})

for color in colors:
    d[color] += 1
# >>> defaultdict(<class 'int'>, {'red': 3, 'green': 2, 'blue': 1})
