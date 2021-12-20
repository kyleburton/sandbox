import sys
# https://ivanyu.me/blog/2013/10/15/z-algorithm/

def z_naive(s):
  Z = [len(s)]
  for k in range(1, len(s)):
    n = 0
    while n + k < len(s) and s[n] == s[n + k]:
      n += 1
    Z.append(n)

  return Z


for s in sys.argv[1:]:
  print "z_naive(%s) = %s" % (s, z_naive(s))
