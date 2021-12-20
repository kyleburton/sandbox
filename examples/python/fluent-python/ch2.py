import os
import dis
import bisect
import sys
import random
from array import array
from random import shuffle
from collections import namedtuple


symbols = '$¢£¥€¤'

ords = tuple(ord(symbol) for symbol in symbols)

print("ords: {}".format(ords))


# NB: 'I' is the storage type for the array
# see:  https://docs.python.org/3/library/array.html
# i is signed int
# I is unsigned int
# l is signed long
# L is unsigned long
things = array('I', (ord(symbol) for symbol in symbols))
print("things: {}".format(things))

colors = ['black', 'white']
sizes = ['S', 'M', 'L', 'XL']

for tshirt in ("{} {}".format(c, s) for c in colors for s in sizes):
    print(tshirt)



lax_coordinates = (33.9425, -118.408056)
city, year, pop, chg, area = ('Tokyo', 2003, 32.450, 0.66, 8014)
traveler_ids = [
    ('USA', '31195855'),
    ('BRA', 'CE342567'),
    ('ESP', 'XDA205856')
]
for passport in sorted(traveler_ids):
    print("{}/{}".format(passport[0], passport[1]))

for country, _ in traveler_ids:
    print(country)


a, b, *rest = range(5)
print("a:{}, b:{}, rest:{}".format(a, b, rest))
_, filename = os.path.split("/home/kburton/.ss/foo.pub")
print("filename: {}".format(filename))

City = namedtuple('City', 'name country year population pop_increase coordinates')
tokyo = City('Tokyo', 'Japan', 2003, 32.450, 0.66, (34.689722, 139.691667))
print("tokyo.population: {}".format(tokyo.population))
print("tokyo.coordinates: {}".format(tokyo.coordinates))
print("fields: {}".format(City._fields))
print("tokyo: {}".format(tokyo))

LatLong = namedtuple('LatLong', 'lat long')
delhi_info = ('Delhi NCR', 'IN', 2003, 21.935, 0.30, LatLong(lat=28.61889, long=77.20889))
delhi = City._make(delhi_info)
print("delhi: {}".format(delhi))
print("splat: {}".format(City(*delhi_info)))

print("delhi._asdict: {}".format(delhi._asdict()))


lst = list(range(11))
shuffle(lst)

pivot_idx = len(l)//2
head, pivot, tail = lst[0:pivot_idx], lst[pivot_idx], lst[pivot_idx+1:]
print("head: {} pivot: {} tail: {}".format(head, pivot, tail))


# "".join([str(x) for x in range(6) for _ in range(11)])
# "".join([str(x) for x in range(10)])[:] * 5

formatted_report = """
0.....6.................................40..........52.55.........
00000000001111111111222222222233333333334444444444555555555566666666667
01234567890123456789012345678901234567890123456789012345678901234567890
1909  Pimoroni PiBrella                      $17.50       3   $52.50
1489  6mm Tactile Switch x20                  $4.95       2    $9.90
1510  Panavise Jr. - PV-201                  $28.00       1   $28.00
1601  PiTFT Mini Kit 230x240                 $34.95       1   $34.95
"""

SKU = slice(0,6)
DESCRIPTION  = slice(6, 40)
UNIT_PRICE = slice(40, 52)
QUANTITY = slice(52, 55)
ITEM_TOTAL = slice(55, None)
line_items = formatted_report.split("\n")
for item in line_items[4:]:
    print("item[UNIT_PRICE]: {}, item[DESCRIPTION]: {}".format(item[UNIT_PRICE], item[DESCRIPTION]))
    


# neat!  you can assign to slices (see p38)
l = list(range(10))
print("before: l: {}".format(l))
l[2:5] = [20, 30]
print("after:  l: {}".format(l))
del l[5:7]
print("after2: l: {}".format(l))
l[3::2] = [11, 22]
print("after3: l: {}".format(l))
l[2:5] = [100]
print("after4: l: {}".format(l))

l = [1, 2, 3]
print("l*5: {}".format(l * 5))
print("string * 5: {}".format("thing" * 5))


def ttt_print_board(b):
    print("-------")
    for row in b:
        print("|", end='')
        for cell in row:
            print(cell, end='')
            print("|", end='')
        print("")
        print("--+-+--")


def ttt_move(b, player, coord):
    x, y = coord
    b[x][y] = player
    return b

board = [[' '] * 3 for i in range(3)]
print("board: {}".format(board))
ttt_print_board(board)
board = ttt_move(board, 'X', (0, 0))
board = ttt_move(board, 'O', (0, 1))
board = ttt_move(board, 'X', (1, 1))
board = ttt_move(board, 'O', (0, 2))
board = ttt_move(board, 'X', (2, 2))
ttt_print_board(board)

# NB: *= may or may not create a new object, see __iadd__ aka "in-place add"
t = (1, 2, 3)
print("id(t): {}".format(id(t)))
t *= 2
print("id(t): {}".format(id(t)))
print("t: {}".format(t))


# :-O pg 43, this expression both fucks up by changing state and
# _also_ raising an excpetion, though I understand the reasoning based
# on the order of evaluation, I still don't like it.

# t = (1, 2, [30, 40])
# t[2] += [50, 60]

# NB: I don't care to hear your raltionalization :)


# dis is neat, reminds me of disassemble (http://clhs.lisp.se/Body/f_disass.htm)
# fire up sbcl and type in:
# (defun myfunc (x) (+ 3 x))
# (disassemble #'myfunc)
dis.dis('s[a] += b')


fruits = "grape,raspberry,apple,banana".split(",")
print("fruits: {}".format(fruits))
print("sorted(fruits): {}".format(sorted(fruits)))
print("sorted(fruits, reverse=True): {}".format(sorted(fruits, reverse=True)))
print("sorted(fruits, key=len): {}".format(sorted(fruits, key=len)))
# and a destructive sort: fruits.sort()



items = [1, 4, 5, 6, 8, 12, 15, 20, 21, 23, 23, 26, 29, 30]
needles = [0, 1, 2, 5, 8, 10, 22, 23, 29, 30, 31]
row_fmt = '{0:2d} @ {1:2d}      {2}{0:<2d}'

def demo(bisect_fn):
    for needle in reversed(needles):
        position = bisect_fn(items, needle)
        offset = position * '  |'
        print(row_fmt.format(needle, position, offset))

bisect_fn = bisect.bisect_left
print("DEMO: {}".format(bisect_fn.__name__))
print("haystack -> {}".format(' '.join('%2d' % n for n in items)))
demo(bisect_fn)



bisect_fn = bisect.bisect_right
print("DEMO: {}".format(bisect_fn.__name__))
print("haystack -> {}".format(' '.join('%2d' % n for n in items)))
demo(bisect_fn)



def grade(score, breakpoints=[60, 70, 80, 85, 90, 95, 100], grades=['F', 'D', 'C', 'B', 'B+', 'A', 'A+', 'A++']):
    idx = bisect.bisect(breakpoints, score)
    # print("score={} idx={}".format(score, idx))
    return grades[idx]

for score in [33, 99, 77, 70, 89, 90, 100]:
    print("grade({:3d}): {:s}".format(score, grade(score)))


SIZE=7
random.seed() # nb: no arg uses some sensible system entropy
my_list = []
for i in range(SIZE):
    new_item = random.randrange(SIZE*2)
    bisect.insort(my_list, new_item)
    print('{:2d} -> {}'.format(new_item, my_list))

print("my_list: {}".format(my_list))

if False:
    floats = array('d', (random.random() for i in range(10**7)))
    print("floats[-1]: {}".format(floats[-1]))
    with open('floats.bin', 'wb') as fh:
        floats.tofile(fh)

    floats2 = array('d')
    with open('floats.bin', 'r') as fh:
        floats.fromfile(fh)
    print("floats2[-1]: {}".format(floats2[-1]))
print("typecode: {}".format(array('d').typecode))
