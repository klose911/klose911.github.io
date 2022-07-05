#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: update_arrays.py

names = ['raymond', 'rachel', 'matthew', 'roger',
         'betty', 'melissa', 'judith', 'charlie']

# ['raymond', 'rachel', 'matthew', 'roger', 'betty', 'melissa', 'judith', 'charlie'] 
del names[0]
#['rachel', 'matthew', 'roger', 'betty', 'melissa', 'judith', 'charlie']

# 下面的代码标志着你用错了数据结构
names.pop(0)
# ['matthew', 'roger', 'betty', 'melissa', 'judith', 'charlie'] 
names.insert(0, 'mark')
#['mark', 'matthew', 'roger', 'betty', 'melissa', 'judith', 'charlie']

 from collections  import deque
names = deque(['raymond', 'rachel', 'matthew', 'roger',
               'betty', 'melissa', 'judith', 'charlie'])
#deque(['raymond', 'rachel', 'matthew', 'roger', 'betty', 'melissa', 'judith', 'charlie']) 
# 用deque更有效率
del names[0]
#deque(['rachel', 'matthew', 'roger', 'betty', 'melissa', 'judith', 'charlie'])
names.popleft()
# deque(['matthew', 'roger', 'betty', 'melissa', 'judith', 'charlie'])
names.appendleft('mark') 
#deque(['mark', 'matthew', 'roger', 'betty', 'melissa', 'judith', 'charlie'])
