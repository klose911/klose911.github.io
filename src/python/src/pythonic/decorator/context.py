#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: context.py

# 保存旧的，创建新的
old_context = getcontext().copy()
getcontext().prec = 50
print(Decimal(355) / Decimal(113)) 
setcontext(old_context)

with localcontext(Context(prec=50)):
    print (Decimal(355) / Decimal(113)) 
