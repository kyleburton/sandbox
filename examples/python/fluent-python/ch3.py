import sys
import re
import collections
import time
from unicodedata import name
from types import MappingProxyType
from collections import abc


my_dict = {}
print("isinstance(my_dict, abc.Mapping): {}".format(isinstance(my_dict, abc.Mapping)))

tt = (1, 2, (30, 40))
print("hash(tt): {}".format(hash(tt)))

tl = (1, 2, [30, 40])
# print("hash(tl): {}".format(hash(tl)))
# => TypeError: unhashable type: 'list'


tf = (1, 2, frozenset([30, 40]))
print("hash(tf): {}".format(hash(tf)))


l1 = ['one', 'two', 'three']
l2 = [1, 2, 3]
res = zip(l1, l2)

# NB: you can only do one of these as it seems to modfiy it's argument :/  Oh, zip objects can be 'consumed'?
#print("list(res): {}".format(list(res)))
print("dict(res): {}".format(dict(res)))

print("res: {}".format(res))



# dict comprehensions - yay!

DIAL_CODES = [
    ( 86, 'China'),
    ( 91, 'India'),
    (  1, 'United States'),
    ( 62, 'Indonesia'),
    ( 55, 'Brazil'),
    ( 92, 'Pakistan'),
    (880, 'Bangladesh'),
    (234, 'Nigeria'),
    (  7, 'Russia'),
    ( 81, 'Japan'),
]

country_code = {country: code for code, country in DIAL_CODES}
print("country_code: {}".format(country_code))

cc2 = {code: country.upper() for code, country in DIAL_CODES}
print("cc2: {}".format(cc2))

WORD_RE = re.compile(r'\w+')
index = {}

with open('/usr/share/dict/words', 'r', encoding='utf-8') as fh:
    for line_no, line in enumerate(fh, 1):
        if line_no > 100:
            break
        for match in WORD_RE.finditer(line):
            word = match.group()
            column_no = match.start()+1
            location = (line_no, column_no)
            occurances = index.get(word, [])
            occurances.append(location)
            index[word] = occurances


for word in sorted(index, key=str.upper):
    print("word: {} {}".format(word, index[word]))




index = {}

with open('/usr/share/dict/words', 'r', encoding='utf-8') as fh:
    for line_no, line in enumerate(fh, 1):
        if line_no > 100:
            break
        for match in WORD_RE.finditer(line):
            word = match.group()
            column_no = match.start()+1
            location = (line_no, column_no)
            occurances = index.get(word, [])
            index.setdefault(word, []).append(location)


for word in sorted(index, key=str.upper):
    print("word: {} {}".format(word, index[word]))


index = collections.defaultdict(list)

with open('/usr/share/dict/words', 'r', encoding='utf-8') as fh:
    for line_no, line in enumerate(fh, 1):
        if line_no > 100:
            break
        for match in WORD_RE.finditer(line):
            word = match.group()
            column_no = match.start()+1
            location = (line_no, column_no)
            occurances = index.get(word, [])
            index[word].append(location)


for word in sorted(index, key=str.upper):
    print("word: {} {}".format(word, index[word]))


class MyDict(collections.UserDict):
    def __missing__(self, key):
        return int(round(time.time() * 1000))


d = MyDict()
print("d: {}".format(d))

print("d[99]: {}".format(d[99]))
print("d['x']: {}".format(d['x']))


d = {1: 'A'}
print("d: {}".format(d))
d_proxy = MappingProxyType(d)
print("d_proxy: {}".format(d_proxy))
print("d_proxy[1]: {}".format(d_proxy[1]))

# the MappingProxyType is r/o
# d_proxy[2] = 'x'
# TypeError: 'mappingproxy' object does not support item assignment

# though if you change the proxied map, you can see it :/
d[2] = 'x'
print("d_proxy: {}".format(d_proxy))


foods = ['spam', 'spam', 'eggs', 'spam']
print("foods: {}".format(foods))
food_set = set(foods)
print("food_set: {}".format(food_set))



s = {1}
print("type(s): {}".format(type(s)))
print("s: {}".format(s))
s.pop()
print("s: {}".format(s))

s = {1, 2, 3}
print("type(s): {}".format(type(s)))
print("s: {}".format(s))
s.pop()
print("s: {}".format(s))


fs = frozenset(range(10))
print("fs: {}".format(fs))

chars = {chr(i) for i in range(32, 256) if 'SIGN' in name(chr(i), '')}
print("chars: {}".format(chars))


