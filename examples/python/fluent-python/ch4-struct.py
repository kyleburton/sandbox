import struct


fmt = '<3s3sHH'
# <      little-endian
# 3s3s   two sequences of 3 bytes
# HH     two 16-bit integers
with open('filter.gif', 'rb') as fp:
    img = memoryview(fp.read())

header = img[:10]
print("bytes(header): {}".format(bytes(header)))

res = struct.unpack(fmt, header)
print("res: {}".format(res))

