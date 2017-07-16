from functools import reduce
from math import hypot


class Vector:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

    def __repr__(self):
        return "Vector({}, {})".format(self.x, self.y)

    def __abs__(self):
        return hypot(self.x, self.y)

    def __bool__(self):
        # return bool(abs(self))
        return bool(self.x or self.y)

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)

    def __mul__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar)

    # TODO: __rmul__


currencies = "".join([chr(x) for x in [36, 162, 163, 165, 8364, 164]])
print("currencies: {}".format(currencies))
print("  codes:    {}".format([ord(x) for x in currencies]))

colors = ['black', 'white']
sizes = ['XS', 'S', 'M', 'L', 'XL']
inventory = [(color, size)
             for color in colors
             for size in sizes]

print("inventory: {}".format(inventory))


def helper(acc, tup):
    acc[tup[0]] = acc.get(tup[0], [])
    acc[tup[0]].append(tup)
    return acc

inventory_by_color = reduce(helper, inventory, {})
print("inventory_by_color: {}".format(inventory_by_color))
