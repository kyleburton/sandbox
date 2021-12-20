import os

home = os.environ.get('HOME')
print "home=%s" % (home)

nothing = os.environ.get('NOTHING')
if nothing is None:
  print "`nothing` is not set (None)"
else:
  print "`nothing`=%s" % (nothing)

