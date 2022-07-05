#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: redirect_stdout.py

# 临时把标准输出重定向到一个文件，然后再恢复正常
with open('help.txt', 'w') as f:
    oldstdout = sys.stdout
    sys.stdout = f
    try:
        help(pow)
    finally:
        sys.stdout = oldstdout

@contextmanager
def redirect_stdout(fileobj):
    oldstdout = sys.stdout
    sys.stdout = fileobj
    try:
        yield fieldobj
    finally:
        sys.stdout = oldstdout

with open('help.txt', 'w') as f:
    with redirect_stdout(f):
        help(pow)
