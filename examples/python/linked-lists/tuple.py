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


def ll_nth(lst, n):
    while n > 0 and lst is not None:
        n = n - 1
        _, lst = lst
    return lst


def ll_del_middle(lst):
    llen = ll_len(lst)
    midx = int(llen/2)
    idx = 0
    nlst = None
    while lst is not None:
        val, lst = lst
        if idx != midx:
            nlst = (val, nlst)
        idx = idx + 1
    return ll_reversed(nlst)


def ll_member(lst, elt):
    found = False
    # print("ll_member: lst={}, elt={}".format(lst, elt))
    while lst:
        val, lst = lst
        if val == elt:
            found = True
            break
    return found


def ll_remove_duplicates(lst):
    nlst = None
    while lst is not None:
        val, lst = lst
        # print("ll_remove_duplicates: checking membership for val={} "
        #       "in nlst={} from lst={}".format(val, nlst, lst))
        if not ll_member(nlst, val):
            nlst = (val, nlst)
    return ll_reversed(nlst)


def ll_is_palindrome(lst):
    # TODO: we really only need to go half way
    rlst = ll_reversed(lst)
    while lst is not None:
        val, lst = lst
        rval, rlst = rlst
        if val != rval:
            return False
    return True


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
l5 = ll_new(1, 2, 3, 4, 5)
print("l5:                    {}".format(l5))
print("ll_nth(l5,0):          {}".format(ll_nth(l5, 0)))
print("ll_nth(l5,1):          {}".format(ll_nth(l5, 1)))
print("ll_nth(l5,2):          {}".format(ll_nth(l5, 2)))
print("ll_nth(l5,3):          {}".format(ll_nth(l5, 3)))
print("ll_nth(l5,4):          {}".format(ll_nth(l5, 4)))
print("ll_nth(l5,5):          {}".format(ll_nth(l5, 5)))
print("ll_nth(l5,6):          {}".format(ll_nth(l5, 6)))
print("ll_del_middle(l5):     {}".format(ll_del_middle(l5)))

# Remove duplicate elements from sorted linked list
l6 = ll_new(1, 2, 3, 4, 5, 5, 5, 5, 1, 6, 7, 7, 8)
print("l6:                        {}".format(l6))
print("ll_member(l6,7)            {}".format(ll_member(l6, 7)))
print("ll_member(l6,9)            {}".format(ll_member(l6, 9)))
print("ll_remove_duplicates(l6):  {}".format(ll_remove_duplicates(l6)))


# Add 1 to a number represented as a linked list
# Reverse a linked list in groups of given size
# Detect loop in linked list
# Remove loop in linked list
# Find nth node from the end of linked list
# Function to check if a singly linked list is a palindrome
l7 = ll_new(1, 2, 3, 4, 3, 2, 1)
print("l7:                      {}".format(l7))
print("ll_is_palindrome(l7):    {}".format(ll_is_palindrome(l7)))
l8 = ll_new(1, 1, 1, 1)
print("l8:                      {}".format(l8))
print("ll_is_palindrome(l8):    {}".format(ll_is_palindrome(l8)))
l9 = ll_new(1, 1, 1, 1, 2)
print("l9:                      {}".format(l9))
print("ll_is_palindrome(l9):    {}".format(ll_is_palindrome(l9)))

# Reverse alternate k node in a singly linked list
# Delete last occurrence of an item from linked list
# Rotate a linked list.
# Delete n nodes after m nodes of a linked list.
# Merge a linked list into another linked list at alternate positions.
# Write a function to delete a linked list.
# Write a function to reverse the nodes of a linked list.
# Why quicksort is preferred for arrays and merge sort for linked lists.
# linked list in java
