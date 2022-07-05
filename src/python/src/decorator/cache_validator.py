#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: cache_validator.py

import time
 
dictcache = {}
 
def cache(func):
    def __decorator(user):    
        now = time.time()
        if (user in dictcache):
            result,cache_time = dictcache[user]
            if (now - cache_time) > 30:  #cache expired
                result = func(user)
                dictcache[user] = (result, now)  #cache the result by user
            else:
                print('cache hits')
        else:
            result = func(user)
            dictcache[user] = (result, now)
        return result      
    return __decorator  
 
def login(user):
    print('in login:' + user)
    msg = validate(user)  
    return msg  
 
@cache  #apply the cache for this slow validation
def validate(user):
    time.sleep(5)  #simulate 10 second block
    msg = "success" if user == "jatsz" else "fail"
    return msg
 
result1 = login('jatsz'); print (result1)  
result2 = login('jatsz'); print (result2)    #this login will return immediately by hit the cache
result3 = login('candy'); print (result3)
