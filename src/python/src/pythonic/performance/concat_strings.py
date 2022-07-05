#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: concat_strings.py

names = ['raymond', 'rachel', 'matthew', 'roger',
         'betty', 'melissa', 'judith', 'charlie']

s = names[0]
for name in names[1:]:
    s += ', ' + name

print (s)

# >>> raymond, rachel, matthew, roger, betty, melissa, judith, charlie

print (', '.join(names))

#>>> raymond, rachel, matthew, roger, betty, melissa, judith, charlie
