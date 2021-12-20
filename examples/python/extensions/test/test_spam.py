import unittest
import doctest

class SpamTest(unittest.TestCase):
  def testImport(self):
    try:
      import spam
    except ImportError, e:
      self.Fail(str(e))

  def testSystem(self):
    import spam
    res = spam.system("ls -l")
    if not res:
      self.Fail("system returned falsy value: %s" % (res))
