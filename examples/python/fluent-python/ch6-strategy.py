from store import Customer, LineItem, Order
from abc import ABC, abstractmethod
from collections import namedtuple


class Promotion(ABC):  # the Strategy: an abstract base class
    @abstractmethod
    def discount(self, order):
        """return discount as a positive dollar amount"""


class FidelityPromo(Promotion):  # first concrete Strategy
    def discount(self, order):
        return order.total() * 0.05 if order.customer.fidelity >= 1000 else 0


class BulkItemPromo(Promotion):  # second concrete Strategy
    def discount(self, order):
        discount = 0
        for item in order.cart:
            if item.quantity >= 20:
                discount += item.total() * 0.1
        return discount


class LargeOrderPromo(Promotion):
    def discount(self, order):
        distinct_items = {item.product for item in order.cart}
        if len(distinct_items) >= 10:
            return order.total() * 0.07
        return 0

jon = Customer("John Doe", 0)
ann = Customer("Ann Smith", 1100)
cart = [
    LineItem('banana',        4,  0.5),
    LineItem('apple',        10,  1.5),
    LineItem('watermellon',   5,  5.0),
]

print("some orders:")
o1 = Order(jon, cart, FidelityPromo())
print("o1: {}".format(o1))
o2 = Order(ann, cart, FidelityPromo())
print("o2: {}".format(o2))

banana_cart = [
    LineItem('banana', 30, 0.5),
    LineItem('apple',  10, 1.5),
]

o3 = Order(jon, banana_cart, BulkItemPromo())
print("o3: {}".format(o3))

long_order = [
    LineItem(str(item_code), 1, 1.0)
    for item_code in range(10)
]

o4 = Order(jon, long_order, LargeOrderPromo())
print("o4: {}".format(o4))

o5 = Order(jon, cart, LargeOrderPromo())
print("o5: {}".format(o5))


################################################################################
# lets use functions for the Stragey pattern /kyle is a fan
print("lets use function for the Stragey pattern")


def fidelity_promo(order):
    return order.total() * 0.05 if order.customer.fidelity >= 1000 else 0


def bulk_item_promo(order):
    discount = 0
    for item in order.cart:
        if item.quantity >= 20:
            discount += item.total() * 0.1
    return discount


def large_order_promo(order):
    distinct_items = {item.product for item in order.cart}
    if len(distinct_items) >= 10:
        return order.total() * 0.07
    return 0


jon = Customer("John Doe", 0)
ann = Customer("Ann Smith", 1100)
cart = [
    LineItem('banana',        4,  0.5),
    LineItem('apple',        10,  1.5),
    LineItem('watermellon',   5,  5.0),
]
print("Order(jon, cart, fidelity_promo): {}".format(
    Order(jon, cart, fidelity_promo))
)

# promos = [bulk_item_promo, large_order_promo, fidelity_promo]
promos = [globals()[name]
          for name in globals()
          if name.endswith('_promo') and name != 'best_promo']


def best_promo(order):
    return max(promo(order) for promo in promos)

o = Order(jon, cart, fidelity_promo)
print("o: {}".format(o))
print("best_promo(o): {:.2f}".format(best_promo(o)))

o2 = Order(ann, cart, fidelity_promo)
print("o2: {}".format(o2))
print("best_promo(o2): {:.2f}".format(best_promo(o2)))
