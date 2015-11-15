#!/usr/bin/env python
# https://docs.python.org/2.3/whatsnew/section-slices.html
import sys


s = "abracadbra"

print "slice[-1] %s" % (s[-1])

print "slice[-2:] %s" % (s[-2:])
print "slice[:] %s" % (s[:])
print "slice[0:-1] %s" % (s[0:-1])
print "slice[::1] %s" % (s[::1])
print "slice[::2] %s" % (s[::2])
print "slice[::3] %s" % (s[::3])
print "slice[::4] %s" % (s[::4])
print "slice[::5] %s" % (s[::5])
for ii in range(1, 2 * len(s)):
  print "slice[::%s] %s" % (ii, s[::ii])

# reverse the list
print "slice[::-1] %s" % (s[::-1])
