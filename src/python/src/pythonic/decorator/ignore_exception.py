#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: ignore_exception.py

try:
    os.remove('somefile.tmp')
except OSError:
    pass

with ignored(OSError):
    os.remove('somefile.tmp')

@contextmanager
def ignored(*exceptions):
    try:
        yield
    except exceptions:
        pass
