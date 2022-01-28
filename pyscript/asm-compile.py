#! /usr/bin/env python3

import os
import sys

fname = sys.argv[1]

ret = os.system("gcc -c {} -o {}".format(fname, fname.replace(".S", ".o")))
if ret != 0:
    sys.exit(-1)

ret = os.system("objdump -d {}".format(fname.replace(".S", ".o")))
sys.exit(ret)
