# import pdb

class ManagedResource():
  def __init__(self, param1):
    print "ManagedResource.__init__"
    self.param1 = param1
  def __enter__(self):
    print "ManagedResource.__enter__"
    return self.param1
  def __exit__(self, typearg, value, traceback):
    # pdb.set_trace()
    print "ManagedResource.__exit__: self=%s type=%s value=%s traceback=%s" % (self, typearg, value, traceback)

with ManagedResource("res1") as res:
  print "res=%s" % (res)
  raise Exception('testing that __exit__ still gets called')
  print "this is after the raise"
