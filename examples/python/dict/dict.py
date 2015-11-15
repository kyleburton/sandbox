d = {"a": 1, "b": 2}

d["c"] = 3

print "for k in d"
for k in d:
  v = d[k]
  print "%s => %s" % (k,v)
print

print "for k,v in d.iteritems()"
for k, v in d.iteritems():
  print "%s => %s" % (k,v)
print

print "for k,v in d.items()"
for k, v in d.items():
  print "%s => %s" % (k,v)
print


print "iter sorted"
for k in sorted(d):
  print "%s => %s" % (k,d[k])
