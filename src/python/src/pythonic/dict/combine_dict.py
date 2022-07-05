#!/usr/bin/python
# -*- coding:utf-8 -*-  
#Filename: combine_dict.py

defaults = {'color': 'red',
            'USERNAME': 'guest'}

import argparse
parser = argparse.ArgumentParser()

parser.add_argument('-u', '-user')
parser.add_argument('-c', '-color')

namespace = parser.parse_args([])
command_line_args = {k: v for k, v in vars(namespace).items() if v}


# 下面是通常的作法，默认使用第一个字典，接着用环境变量覆盖它，最后用命令行参数覆盖它。

# 然而不幸的是，这种方法拷贝数据太疯狂。

d = defaults.copy()
for k, v in d.items():
    print(k, '->', v)
# color -> red
# USERNAME -> guest

d.update(os.environ)
for k, v in d.items():
    print(k, '->', v)
# color -> red
# USERNAME -> klose.wu
# .......
# TMP -> C:\Users\klose.wu\AppData\Local\Temp
# TMPDIR -> C:\Users\klose.wu\AppData\Local\Temp
# USERDNSDOMAIN -> EBAOTECH.COM
# USERDOMAIN -> EBAOTECH
# USERPROFILE -> C:\Users\klose.wu
# VBOX_MSI_INSTALL_PATH -> C:\Program Files\Oracle\VirtualBox\
# VSEDEFLOGDIR -> C:\ProgramData\McAfee\DesktopProtection
# WINDIR -> C:\Windows

d.update(command_line_args)
for k, v in d.items():
    print(k, '->', v)
# color -> red
# USERNAME -> klose.wu
# .......
# TMP -> C:\Users\klose.wu\AppData\Local\Temp
# TMPDIR -> C:\Users\klose.wu\AppData\Local\Temp
# USERDNSDOMAIN -> EBAOTECH.COM
# USERDOMAIN -> EBAOTECH
# USERPROFILE -> C:\Users\klose.wu
# VBOX_MSI_INSTALL_PATH -> C:\Program Files\Oracle\VirtualBox\
# VSEDEFLOGDIR -> C:\ProgramData\McAfee\DesktopProtection
# WINDIR -> C:\Windows

# 更高效优雅的写法
from collections import ChainMap

d = ChainMap(command_line_args, os.environ, defaults)
for k, v in d.items():
    print(k, '->', v)
# color -> red
# USERNAME -> klose.wu
# .......
# TMP -> C:\Users\klose.wu\AppData\Local\Temp
# TMPDIR -> C:\Users\klose.wu\AppData\Local\Temp
# USERDNSDOMAIN -> EBAOTECH.COM
# USERDOMAIN -> EBAOTECH
# USERPROFILE -> C:\Users\klose.wu
# VBOX_MSI_INSTALL_PATH -> C:\Program Files\Oracle\VirtualBox\
# VSEDEFLOGDIR -> C:\ProgramData\McAfee\DesktopProtection
# WINDIR -> C:\Windows
