d = {"a": 1, "b": 2}

d["c"] = 3

print("for k in d")
for k in d:
  v = d[k]
  print("%s => %s" % (k,v))
print

try:
  print("for k,v in d.iteritems()")
  for k, v in d.iteritems():
    print("%s => %s" % (k,v))
  print
except:
  print("dict.iteritems is supported in python2, but not python3")

print("for k,v in d.items()")
for k, v in d.items():
  print("%s => %s" % (k,v))
print


print("iter sorted")
for k in sorted(d):
  print("%s => %s" % (k,d[k]))

try:
  print("d['missing']={}".format(d["missing"]))
except:
  print("can't access a key that's not there")


print("d.get('missing')={}".format(d.get("missing")))
