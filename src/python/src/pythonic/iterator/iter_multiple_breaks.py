#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: iter_multiple_breaks.py

seq = [0, 1, 2, 3, 4, 5, 6]

def find(seq, target):
    found = False

    for i, value in enumerate(seq):
        if value == target:
            found = True
            break
        
    if not found:
        return -1

    return i

print (find(seq, 3)) # 3 
print (find(seq, 8)) # -1 


def find(seq, target):
    for i, value in enumerate(seq):
        if value == target:
            break
        
    else:
        return -1
    
    return i

print (find(seq, 3)) # 3 
print (find(seq, 8)) # -1 
