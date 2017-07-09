# tuples
def ll_new(*elts):
    lst = None
    for elt in reversed(elts):
        lst = (elt, lst)
    return lst


def ll_to_array(lst):
    ary = []
    while lst is not None:
        head, lst = lst
        ary.append(head)
    return ary


def ll_len(lst):
    len = 0
    while lst is not None:
        head, lst = lst
        len = len + 1
    return len


def ll_reversed(lst):
    l2 = None
    while lst is not None:
        head, lst = lst
        l2 = (head, l2)
    return l2


def ll_del_next(lst):
    a, tail = lst
    _, tail = tail
    return (a, tail)


def ll_push(lst, elt):
    return (elt, lst)


def ll_end(lst):
    curr = lst
    while True:
        curr = lst
        head, lst = lst
        if lst is None:
            return curr
    else:
        return None


def ll_mid(lst):
    rabbit = lst
    tortise = lst
    while rabbit is not None:
        _, rabbit = rabbit
        if rabbit is None:
            break
        _, rabbit = rabbit
        _, tortise = tortise
    return tortise


def ll_concat(l1, l2):
    l1 = ll_reversed(l1)
    while l2 is not None:
        head, l2 = l2
        l1 = (head, l1)
    return ll_reversed(l1)


def ll_flatten(lst, newl=None):
    while lst is not None:
        head, lst = lst
        if type(head) is tuple:
            head = ll_flatten(head)
            newl = ll_concat(head, newl)
        else:
            newl = (head, newl)
    return ll_reversed(newl)


def ll_filter(lst, p):
    newl = None
    while lst:
        head, lst = lst
        if p(head):
            newl = (head, newl)
    return newl


def is_zero(n):
    return n == 0


def compliment(f):
    def inner(*args):
        return not f(*args)
    return inner


l1 = ll_new(1, 2, 3, 4, 5)
print("l1:               {}".format(l1))
print("ll_len(l1):       {}".format(ll_len(l1)))
print("ll_to_array(l1):  {}".format(ll_to_array(l1)))
print("ll_reversed(l1):  {}".format(ll_reversed(l1)))
print("ll_del_next(l1):  {}".format(ll_del_next(l1)))
print("ll_push(l1, 'x'): {}".format(ll_push(l1, 'x')))
print("ll_end(l1):       {}".format(ll_end(l1)))


# Print the Middle of a given linked list
l2 = ll_new(1, 2, 3, 4, 5)
print("l2:             {}".format(l2))
print("ll_mid(l2):     {}".format(ll_mid(l2)))

l3 = ll_new(1, 2, 3, 4, 5, 6)
print("l3:             {}".format(l3))
print("ll_mid(l3):     {}".format(ll_mid(l3)))

print("ll_concat(..):  {}".format(ll_concat(ll_new(1, 2, 3), ll_new(4, 5, 6))))

# Flattening a linked list
l4 = ll_new(
    1,
    ll_new('a', 'b', 'c'),
    3,
    4,
    ll_new('!', '@', '#',
           ll_new('{', '}', '<')),
    6)

print("ll_flatten(..):      {}".format(ll_flatten(ll_new(1, ll_new(2)))))
print("l4:              {}".format(l4))
print("ll_flatten(..):  {}".format(ll_flatten(ll_new(1, 2, 3, 4,
                                                     ll_new(5, 6, 7,
                                                            ll_new(8, 9))))))
print("ll_flatten(l4):  {}".format(ll_flatten(l4)))
print("ll_to_array(l4): {}".format(ll_to_array(ll_flatten(l4))))

# Delete the elements in an linked list whose sum is equal to zero

print("ll_filter(.., is_zero):               {}".format(
    ll_filter(ll_new(1, 2, 3, 0, 4, 5, 0, 6, 0, 0, 0, 7, 8, 9),
              is_zero)
))
print("ll_filter(.., compliment(is_zero)):   {}".format(
    ll_filter(ll_new(1, 2, 3, 0, 4, 5, 0, 6, 0, 0, 0, 7, 8, 9),
              compliment(is_zero))
))

# Delete middle of linked list
# Remove duplicate elements from sorted linked list
# Add 1 to a number represented as a linked list
# Reverse a linked list in groups of given size
# Detect loop in linked list
# Remove loop in linked list
# Find nth node from the end of linked list
# Function to check if a singly linked list is a palindrome
# Reverse alternate k node in a singly linked list
# Delete last occurrence of an item from linked list
# Rotate a linked list.
# Delete n nodes after m nodes of a linked list.
# Merge a linked list into another linked list at alternate positions.
# Write a function to delete a linked list.
# Write a function to reverse the nodes of a linked list.
# Why quicksort is preferred for arrays and merge sort for linked lists.
# linked list in java
