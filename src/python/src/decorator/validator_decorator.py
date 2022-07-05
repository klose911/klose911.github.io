#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: validator_decorator.py

def printdebug(func):
    def __decorator(user):    
        print('enter the login')
        result = func(user) 
        print('exit the login')
        return result      
    return __decorator  
 
def login(user):
    print('in login:' + user)
    msg = validate(user)  #抽取要应用修饰器的方法
    return msg  
 
@printdebug  #对validate函数应用修饰器
def validate(user):
    msg = "success" if user == "jatsz" else "fail"
    return msg
 
result1 = login('jatsz');
print (result1)
