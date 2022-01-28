#! /usr/bin/env python3

import os
import signal
import sys


def sig_exit(signum, empty):
    print("Exiting on Signal({})".format(str(signum)))
    sys.exit(-1)


signal.signal(signal.SIGINT, sig_exit)

# Configurable
impls = ["old", "new"]
files_out = [
    "strchr{}", "strchrnul{}", "wcschr{}", "wcschrnul{}", "strlen{}",
    "strnlen{}", "wcslen{}", "wcsnlen{}"
]
funcs = ["strchr", "strlen"]
exts = ["avx2", "evex"]
# Hard coded constants

root = "/home/noah/programs/opensource/glibc-dev/"
from_path = root + "/glibc-versions/{}/*-{}-{}.S"
to_path = root + "/src/glibc/sysdeps/x86_64/multiarch/{}-{}.S"

glibc_path = root + "/build/glibc/"
bench_path = glibc_path + "benchtests/"
result_path = root + "/results/{}/"

rerun_cmd = "make -r -C " + root + "/src/glibc/string/ objdir=`pwd` check"
build_cmd = "rm -rf " + glibc_path + "; mkdir -p " + glibc_path + "; (cd " + glibc_path + "; unset LD_LIBRARY_PATH; " + glibc_path + "/src/glibc/configure --prefix=/usr; make -j 7 --silent)"
test_cmd = "(cd " + glibc_path + "; unset LD_LIBRARY_PATH; make xcheck --silent)"
bench_cmd = "(cd " + glibc_path + "; unset LD_LIBRARY_PATH; taskset -c 0 make --silent bench BENCHSET=\"string-benchset wcsmbs-benchset\")"

file_prefix = "bench-"
file_postfix = ".out"


def copy(impl, fake):
    for func in funcs:
        for ext in exts:
            full_from_path = from_path.format(impl, func, ext)
            full_to_path = to_path.format(func, ext)
            print("Running: " +
                  "cp {} {}".format(full_from_path, full_to_path))
            if fake is False:
                assert os.system("cp {} {}".format(full_from_path,
                                                   full_to_path)) == 0


def build(impl):
    copy(impl, False)
    os.system(build_cmd)


def test():
    os.system(test_cmd)


def bench(impl):
    impl_result_path = result_path.format(impl)
    os.system("mkdir -p {}".format(impl_result_path))
    for func in funcs:
        for ext in exts:
            os.system("mkdir -p {}/{}-{}".format(impl_result_path, func, ext))

    impl_files = []
    for f in files_out:
        impl_files.append(file_prefix + f + file_postfix)

    for i in range(0, 5):
        print("Running: {} - {}".format(impl, i))
        os.system(bench_cmd)

        for f in impl_files:
            src = bench_path + f.format("")
            dst = impl_result_path + f.format(i)
            os.system("cp {} {}".format(src, dst))


if len(sys.argv) > 1:
    print(rerun_cmd)
    print(build_cmd)
    print(test_cmd)
    print(bench_cmd)
    for impl in impls:
        copy(impl, True)
    sys.exit(0)

for impl in impls:
    build(impl)
    bench(impl)
