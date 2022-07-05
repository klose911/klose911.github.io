#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: print_debug_decorator.py

def printdebug(func):
    def __decorator():
        print('enter the login')
        func()
        print('exit the login')
    return __decorator  

@printdebug  #把login做为func的实际参数传入printdebug的
def login():
    print('in login')
    
login()  #使得调用更加自然
