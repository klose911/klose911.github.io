#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: decorator_return_result.py

def printdebug(func):
    def __decorator(user):    
        print('enter the login')
        result = func(user)  
        print('exit the login')
        return result        #在装饰器函数返回调用func的结果
    return __decorator  
 
@printdebug 
def login(user):
    print('in login:' + user)
    msg = "success" if user == "jatsz" else "fail"
    return msg  # login函数返回结果
 
result1 = login('jatsz')
print(result1) #success 
 
result2 = login('candy')
print (result2) #fail
