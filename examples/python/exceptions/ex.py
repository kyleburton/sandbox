#!/usr/bin/env python
import sys



while True:
  try:
    x = int(raw_input("Please enter a number: "))
    break
  except ValueError as e:
    #import pdb; pdb.set_trace()
    print "Oops! invalid number, try again. %s" % (e.message)
    print "Oops! invalid number, try again. %s" % (sys.exc_info()[0])


try:
  raise Exception("an exception")
except Exception as e:
  print "caught it '%s'" % (e)
