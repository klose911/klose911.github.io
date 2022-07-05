#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: decorator_with_param.py

def printdebug_level(level):  #通过wrapper来增加装饰器的参数
    def printdebug(func):
        def __decorator(user):    
            print('enter the login, and debug level is: ' + str(level)) #打印debug等级
            func(user)  
            print('exit the login')
        return __decorator  
    return printdebug    #返回原始的装饰器
 
@printdebug_level(level=5)   #传入装饰器的debug等级参数为5
def login(user):
    print('in login:' + user)
 
login('jatsz') #等价于printdebug_level(5) (login) ('jatsz') 
