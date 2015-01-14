#!/usr/bin/env python

# Copyright (c) 2015 Linus Yang

import os
import re
import datetime

def main():
    header = 'Linus Yang'
    current_dir = os.path.dirname(os.path.realpath(__file__))
    for name in os.listdir(current_dir):
        fpath = os.path.join(current_dir, name)
        if os.path.isfile(fpath) and re.search(r'\.(py|cpp|hs|c|java|cxx|h|js)$', name):
            f = open(fpath, "r+")
            content = f.read()
            f.seek(0)
            if content.find(header) == -1:
                year = str(datetime.datetime.now().year)
                header_year = 'Copyright (c) %s %s' % (year, header)
                if name.find('.py') != -1:
                    content = ('# %s\n\n' % header_year) + content
                elif name.find('.cpp') != -1:
                    content = ('// %s\n\n' % header_year) + content
                elif name.find('.c') != -1:
                    content = ('/* %s */\n\n' % header_year) + content
                elif name.find('.hs') != -1:
                    content = ('-- %s\n\n' % header_year) + content
                f.write(content)
            f.close()

if __name__ == '__main__':
    main()
