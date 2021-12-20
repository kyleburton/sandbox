import array
import numpy
import random
import os
from time import perf_counter as pc

print("\n                    START                    ".format())
print("_" * 45)

# memoryview is about memory efficiency, feels similar to go's slices.
# is this another example accidental complexity in python?

numbers = array.array('h', [-2, -1, 0, 1, 0])
print("numbers: {}".format(numbers))
memv = memoryview(numbers)

print("memv: {}".format(memv))
print("len(memv): {}".format(len(memv)))
print("memv[0]: {}".format(memv[0]))
memv_oct = memv.cast('B')
print("memv_oct: {}".format(memv_oct.tolist()))

memv_oct[5] = 4
# NB: this changes the 5th byte in the numbers array, which are signed shorts, of 2 bytes
# so offsedt 5 is the first byte of the 3rd element (the zero).  So we get:
# 00000000 00000000 => 0
# 00000100 00000000 => 1024 (aka 2^10)
print("numbers: {}".format(numbers))

a = numpy.arange(12)
print("numpy.arange(12): {}".format(a))
print("type(a): {}".format(a))
print("a.shape: {}".format(a.shape))

# change the matrix to 3 vectors of 4 items
a.shape = 3, 4
print("a: {}".format(a))
print("type(a): {}".format(a))
print("a.shape: {}".format(a.shape))
print("a[2]: {}".format(a[2]))
print("a[:, 1]: {}".format(a[:, 1]))
# a.transpose()  # NB: for some reason transpose isn't destructive? lovely, lovely consistency :/
print("a.transpose(): {}".format(a.transpose()))


def make_floats_file(fname, nitems):
    with open(fname, 'w') as fh:
        for idx in range(nitems):
            print("{}".format(str(random.random())), file=fh)

    return True

if not os.path.isfile('floats-10M-lines.txt'):
    # make_floats_file('floats-10M-lines.txt', 10000000)
    make_floats_file('floats-10M-lines.txt', 1000)

floats = numpy.loadtxt('floats-10M-lines.txt')
print("floats[-3:]: {}".format(floats[-3:]))
floats *= 0.5
print("floats[-3:]: {}".format(floats[-3:]))
stime = pc()
floats /= 3
etime = pc()
print("stime: {}; etime: {}; elapsed: {}".format(stime, etime, etime-stime))
numpy.save('floats.bin', floats)
# WOW, super awesome that it automatically appends a .npy to the file name
floats2 = numpy.load('floats.bin.npy', 'r+')
floats2 *= 6
print("floats2[-3:]: {}".format(floats2[-3:]))
