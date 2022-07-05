#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: dict_group.py

names = ['raymond', 'rachel', 'matthew', 'roger',
         'betty', 'melissa', 'judith', 'charlie']

# 在这个例子，我们按 name 的长度分组 
d = {}

for name in names:
    key = len(name)
    if key not in d:
        d[key] = []
    d[key].append(name)
# >>> {7: ['raymond', 'matthew', 'melissa', 'charlie'], 6: ['rachel', 'judith'], 5: ['roger', 'betty']}


d = {}

for name in names:
    key = len(name)
    d.setdefault(key, []).append(name)
# >>> {7: ['raymond', 'matthew', 'melissa', 'charlie'], 6: ['rachel', 'judith'], 5: ['roger', 'betty']}

from collections import defaultdict

d = defaultdict(list)
#>>> defaultdict(<class 'list'>, {})

for name in names:
    key = len(name)
    d[key].append(name)
#defaultdict(<class 'list'>, {7: ['raymond', 'matthew', 'melissa', 'charlie'], 6: ['rachel', 'judith'], 5: ['roger', 'betty']})
