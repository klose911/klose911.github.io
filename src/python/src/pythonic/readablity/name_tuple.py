#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: name_tuple.py

from collections import namedtuple

TestResults = namedtuple('TestResults', ['failed', 'attempted'])
testResult = TestResults(failed=0, attempted=4)
#TestResults(failed=0, attempted=4)
