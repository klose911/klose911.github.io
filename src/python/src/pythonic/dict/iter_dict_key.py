#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: iter_dict_key.py


d = {'matthew': 'blue', 'rachel': 'green', 'raymond': 'red'}

for k in d:
    print (k)

# >>> matthew
# rachel
# raymond

for k in list(d.keys()):
    if k.startswith('r'):
        del (d[k])

# >>> matthew

