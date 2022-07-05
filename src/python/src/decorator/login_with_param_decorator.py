#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: login_with_param_decorator.py
def printdebug(func):
    def __decorator(user):    # 增加传递给login的参数
        print('enter the login')
        func(user)  # login调用带上参数
        print('exit the login')
    return __decorator  
 
@printdebug 
def login(user):
    print('in login:' + user)
 
login('jatsz')  # 真实调用是__decorator(login)(user) 
