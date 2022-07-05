#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: multiple_decorators.py

def printdebug(func):
    def __decorator():    
        print('enter the login')
        func() 
        print('exit the login')
    return __decorator  
 
def others(func):    
    def __decorator():
        print ('***other decorator***')
        func()
    return __decorator
 
@others         #相当于others(printdebug(login)) ()
@printdebug
def login():
    print('in login:')
 
@printdebug    #相当于printdebug(others(login)) ()
@others
def logout():
    print('in logout:')
 
login()
print('---------------------------') 
logout()
