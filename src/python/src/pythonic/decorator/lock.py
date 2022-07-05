#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: lock.py

# 创建锁
lock = threading.Lock()

# 使用锁的老方法
lock.acquire()
try:
    print 'Critical section 1'
    print 'Critical section 2'
finally:
    lock.release()

# 使用锁的新方法
with lock:
    print 'Critical section 1'
    print 'Critical section 2'
