import sys

for arg in sys.argv[1:]:
  print "%s len=%s" % (arg, len(arg))
  for idx, ch in enumerate(arg):
    print "  s[%s] = %s" % (idx, ch)
