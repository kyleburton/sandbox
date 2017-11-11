from store import Customer, LineItem, Order


def decorate(f):
    f.__cache = []

    def wrapped(**args):
        res = f(**args)
        f.__cache.append((args, res))
        if len(f.__cache) > 10:
            f.__cache.pop()
        return res
    wrapped.__cache = f.__cache
    return wrapped


@decorate
def target():
    print("running target")


print("target(): {}".format(target()))
print("last 10: {}".format(target.__cache))
print("target(): {}".format(target()))
print("last 10: {}".format(target.__cache))
print("target(): {}".format(target()))
print("last 10: {}".format(target.__cache))

registry = []


def register(f):
    print("running register({})".format(f))
    registry.append(f)
    return f


@register
def f1():
    print('in f1()')


@register
def f2():
    print('in f2()')


def f3():
    print('in f3()')


promotions = []


def promotion(promo_func):
    promotions.append(promo_func)
    return promo_func

@promotion
def fidelity_promo(order):
    """5% discount for customers with 1000 or more fidelity points"""
    return order.total() * 0.05 if order.customer.fidelity >= 1000 else 0

@promotion
def bulk_item(order):
    """10% discount for each line item with 20 or more units"""
    discount = 0
    for item in order.cart:
        if item.quantity >= 20:
            discount += item.total() * 0.1
    return discount

@promotion
def large_order(order):
    """7% discount for orders with 10 or more distinct items"""
    distinct_items = {item.product for item in order.cart}
    if len(distinct_items) >= 10:
        return order.total() * 0.07
    return 0

def best_promo(order):
    """Select the best promotion (discount) available for the order"""
    return max(promo(order) for promo in promotions)
    



def main2():
    print("in main()")
    print("registry: {}".format(registry))
    f1()
    f2()
    f3()

def main():
    jon = Customer("John Doe", 0)
    ann = Customer("Ann Smith", 1100)
    cart = [
        LineItem('banana',        4,  0.5),
        LineItem('apple',        10,  1.5),
        LineItem('watermellon',   5,  5.0),
    ]
    o = Order(jon, cart, fidelity_promo)
    o2 = Order(ann, cart, fidelity_promo)
    print("o:  {}".format(o))
    print("o2: {}".format(o2))
    print("best_promo(o):  {}".format(best_promo(o)))
    print("best_promo(o2): {}".format(best_promo(o2)))


if __name__ == '__main__':
    main()
