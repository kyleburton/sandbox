from collections import namedtuple

Customer = namedtuple('Customer', 'name fidelity')


class LineItem:
    def __init__(self, product, quantity, price):
        self.product = product
        self.quantity = quantity
        self.price = price

    def total(self):
        return self.quantity * self.price


class Order:  # the Context
    def __init__(self, customer, cart, promotion=None):
        self.customer = customer
        self.cart = list(cart)
        self.promotion = promotion

    def total(self):
        return sum(item.total() for item in self.cart)

    def due(self):
        return self.total() - self.discount()

    def discount(self):
        if self.promotion is None:
            return 0
        if callable(self.promotion):
            return self.promotion(self)
        return self.promotion.discount(self)

    def __repr__(self):
        fmt = '<Order total: {:.2f} due: {:.2f} promo:{} discount: {:.2f}>'
        return fmt.format(
            self.total(),
            self.due(),
            self.promotion.__class__.__name__,
            self.discount())


