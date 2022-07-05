#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: print_debug_with_function.py
def login():
    print('in login')
    
def printdebug(func):
    def __decorator():
        print('enter the login')
        func()
        print('exit the login')
    return __decorator  #函数作为返回值

debug_login = printdebug(login)  #函数赋值给变量

debug_login()  #通过变量来调用作为返回值的函数
