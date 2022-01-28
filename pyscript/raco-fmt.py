#! /usr/bin/env python3

import sys
from datetime import datetime
import os


def unique_fname():
    now_str = str(datetime.now())
    now_str = now_str.replace(" ",
                              "-").replace(":", "-").replace(".", "-").replace(
                                  ":", "-").lstrip().rstrip()
    return now_str


def write_to_file(fname, content):
    tmp_path = "/home/noah/tmp/raco-fmt/"

    if not os.path.isdir(tmp_path):
        os.system("mkdir -p {}".format(tmp_path))

    fname = tmp_path + fname
    assert not os.path.exists(fname)
    try:
        f = open(fname, "w+")
        f.write(content)
        f.flush()
        f.close()
    except IOError:
        assert False, "Error opening {}".format(fname)

    return fname


def run_raco_fmt(fname):
    assert os.path.exists(fname)
    ret = os.system("raco fmt {}".format(fname))
    return ret


fname = unique_fname()
fname = write_to_file(fname, sys.stdin.read())

assert 0 == run_raco_fmt(fname)
