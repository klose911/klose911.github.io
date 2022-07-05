#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: iter_dict.py


d = {'matthew': 'blue', 'rachel': 'green', 'raymond': 'red'}

# 并不快，每次必须要重新哈希并做一次查找 
for k in d:
    print (k, '->', d[k]) 
# >>> matthew -> blue
# rachel -> green
# raymond -> red

# for k, v in d.iteritems():
#     print (k, '->', v) 

for k, v in d.items():
    print (k, '->', v)

