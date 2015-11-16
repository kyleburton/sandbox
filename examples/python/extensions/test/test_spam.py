import unittest
import doctest

class SpamTest(unittest.TestCase):
  def runTest(self):
    try:
      import spam
    except ImportError, e:
      self.Fail(str(e))

