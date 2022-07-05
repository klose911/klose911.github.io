#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: cache.py

# 混着业务和管理逻辑，无法重用
def web_lookup(url, saved={}):
    if url in saved:
        return saved[url]
    page = urllib.urlopen(url).read()
    saved[url] = page
    return page

@cache
def web_lookup(url):
    return urllib.urlopen(url).read()
