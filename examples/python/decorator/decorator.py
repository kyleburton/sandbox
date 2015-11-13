# http://simeonfranklin.com/blog/2012/jul/1/python-decorators-in-12-steps/

def foo():
  pass

print "foo's class=%s" % (foo.__class__)


def add(x,y):
  return x+y

def apply(func, x, y):
  return func(x,y)

print "apply(add,1,20)=%s" % (apply(add,1,2))



def makeAdder(x):
  def adder(y):
    return x+y;
  return adder

add3 = makeAdder(3)
print "add3(11)=%s" % (add3(10))

def wrap_with_print(func):
  def inner(*args,**kwargs):
    res = None
    try:
      print "wrap_with_print: calling f with args: %s %s" % (args, kwargs)
      res = func(*args,**kwargs)
      print "wrap_with_print: f returned %s" % (res)
      return res
    except:
      print "wrap_with_print: wrapped function threw!"
      raise
  return inner

@wrap_with_print
def myfunc(a,b):
  print "myfunc(%s,%s)" % (a,b)
  return a+b

res = myfunc(5,6)
print "myfunc(5,6) = %s" % (res)

