def cons(a, b):
    return (a, b)


def car(c):
    return c[0]


def cdr(c):
    return c[1]


def ll_new(*elts):
    lst = None
    for elt in elts:
        lst = cons(elt, lst)
    return lst


def ll_to_array(lst):
    ary = []
    while lst is not None:
        ary.append(car(lst))
        lst = cdr(lst)
    return ary


l1 = ll_new(1, 2, 3, 4, 5)
print("l1:               {}".format(l1))
print("ll_to_array(l1):  {}".format(ll_to_array(l1)))
