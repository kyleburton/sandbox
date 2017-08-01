from collections import namedtuple
from operator import mul, itemgetter, attrgetter, methodcaller
from functools import reduce, partial, partialmethod
import functools
import operator
import unicodedata
# nope: tagger is python 2 only, last updated in 2006
#  from tagger import tag

def fact(n):
    return reduce(lambda acc, num: acc * num, range(1, n+1))


def fact2(n):
    return reduce(mul, range(1, n+1))



print("fact(5):  {}".format(fact(5)))
print("fact2(5): {}".format(fact2(5)))


metro_data = [
    ('Tokyo',           'JP', 36.933, (35.680277, 139.691667)),
    ('Delhi NCR',       'IN', 21.935, (28.613889, 77.208889)),
    ('Meixco City',     'MX', 20.142, (19.433333, -99.133333)),
    ('New York-Newark', 'US', 20.104, (40.808611, -74.020386)),
    ('Sao Paulo',       'BR', 19.649, (-23.547778, -46.635833)),
]

print("by country code")
for city in sorted(metro_data, key=itemgetter(1)):
    print("city: {}".format(city))

print("by population (millions)")
for city in sorted(metro_data, key=itemgetter(2)):
    print("city: {}".format(city))

print("by coordinates (lon,lat)")
for city in sorted(metro_data, key=itemgetter(3)):
    print("city: {}".format(city))


cc_name = itemgetter(1, 0)
for city in metro_data:
    print("cc_name(city): {}".format(cc_name(city)))

    
LatLong = namedtuple("LatLong", 'lat long')
Metropolis = namedtuple("Metropolis", 'name cc pop coord')

for metro in metro_data:
    print("len(metro): {}".format(len(metro)))

for name, cc, pop, (lat,lon) in metro_data:
    print("name={}; cc={}; pop={}; ll={}".format(name, cc, pop, (lat,lon)))

    

metro_areas = [Metropolis(name, cc, pop, LatLong(lat, lon)) 
               for name, cc, pop, (lat, lon) in metro_data]
print("metro_areas[0]: {}".format(metro_areas[0]))

name_lat = attrgetter('name', 'coord.lat')
for cit in sorted(metro_areas, key=attrgetter('coord.lat')):
    print("name_lat(cit): {}".format(name_lat(cit)))


print("things in operator: {}".format([name for name in sorted(dir(operator)) if not name.startswith("_")]))

s = 'The time has come'
upcase = methodcaller('upper')
print("upcase(s): {}".format(upcase(s)))
hiphenate = methodcaller('replace', ' ', '-')
print("hiphenate(s): {}".format(hiphenate(s)))


####################
for elt in [name for name in sorted(dir(functools)) if not name.startswith("_")]:
    print("functools.{}".format(elt))


# ok, curry/cut is called partial / partialmethod
triple = partial(mul, 3)
print("triple(7): {}".format(triple(7)))

print("list(map(triple, range(1, 10))): {}".format(list(map(triple, range(1, 10)))))
nfc = functools.partial(unicodedata.normalize, 'NFC')
s1 = 'café'
s2 = 'cafe\u0301'
tup1 = (s1, s2)
print("tup1: {}".format(tup1))
print("s1==s1: {}".format(s1==s2))
print("nfc(s1)==nfc(s1): {}".format(nfc(s1)==nfc(s2)))

# nope: tagger is python 2 only, last updated in 2006
def tag(tname, body=None, **attrs):
    # TODO[optional]: escape single/double quotes in the attrs
    attr_str = " ".join(["{}='{}'".format(k,v) for k,v in attrs.items()])
    if not body:
        return "<{} {}/>".format(tname, attr_str)

    return "<{} {}>{}</{}>".format(tname, attr_str, body, tname)

print("tag('img'): {}".format(tag('img')))
print("tag('img', src='wumpus.jpeg'): {}".format(tag('img', src='wumpus.jpeg')))
print("tag('p', 'content goes here', cls='bold'): {}".format(tag('p', 'content goes here', cls='bold')))

picture = partial(tag, 'img', cls='pic-frame')
print("picture(src='wumpus.jpeg'): {}".format(picture(src='wumpus.jpeg')))
print("picture: {}".format(picture))
print("picture.func: {}".format(picture.func))
print("picture.args: {}".format(picture.args))
print("picture.keywords: {}".format(picture.keywords))
