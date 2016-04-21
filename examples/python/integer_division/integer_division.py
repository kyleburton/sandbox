def intdiv_slow(num, denom):
    if denom == 0:
        return (None, None)

    nsign = 1
    if num < 0:
        num *= -1
        nsign = -1

    dsign = 1
    if denom < 0:
        denom *= -1
        dsign = -1

    times = 0
    while num >= denom:
        num -= denom
        times = times + 1

    return (nsign * dsign * times, None)


def set_bit(v, index, x):
    """Set the index:th bit of v to x, and return the new value."""
    mask = 1 << index
    v &= ~mask
    if x != 0:
        v |= mask
    return v


def get_bit(v, index):
    if v & (1 << index) == 0:
        return 0
    else:
        return 1


def num_bits(n):
    nb = 0
    while n != 0:
        n = n >> 1
        nb += 1
    return nb


# https://en.wikipedia.org/wiki/Division_algorithm#Integer_division_.28unsigned.29_with_remainder
def intdiv_binary_long(N, D):
    if D == 0:
        return (None, None)

    sD = 1
    sN = 1

    if D < 0:
        sD = -1
        D = D * -1

    if N < 0:
        sN = -1
        N = N * -1

    Q = 0                           # initialize quotient and remainder to zero
    R = 0
    numBits = num_bits(N)
    # print("intdiv_binary_long({}, {}) numBits={} indexes={}".format(
    #     num,
    #     denom,
    #     numBits,
    #     list(range(numBits - 1, -1, -1))
    # ))
    for i in range(numBits - 1, -1, -1):   # where n is number of bits in N
        # print("  R={} R<<1 = {}".format(R, R << 1))
        R = R << 1    # left-shift R by 1 bit
        # R(0) = N(i) # set the least-significant bit of R equal to bit i of the  numerator # NOQA
        R = set_bit(R, 0, get_bit(N, i))
        # print("  set bit 0 of R to {}th bit (value={}) :: R={}".format(
        #     i,
        #     get_bit(N, i),
        #     R))
        if R >= D:
            R = R - D
            # Q(i) = 1
            Q = set_bit(Q, i, 1)
            # print("  set bit {} of Q to 1 :: Q={} {}".format(
            #     i,
            #     Q,
            #     bin(Q)))
        # print("  R={}".format(R))
        # print("  Q={}".format(Q))

    return (Q * sD * sN, R)


tests_full = [
    (0, 0, None),
    (0, 1, 0),
    (1, 0, None),
    (1, 1, 1),
    (2, 1, 2),
    (1, 2, 0),
    (10, 5, 2),
    (11, 5, 2),
    (-11, 5, -2),
    (-10, -5, 2),
    (1000000, 1000, 1000),
    (-3, 1, -3),
    (-1, 3, 0),
]

tests_short = [
    (12, 4, 3)
]

tests = tests_full

fn = intdiv_binary_long
for (num, denom, expected) in tests:
    (actual, rem) = fn(num, denom)
    print("{}: intdiv({}, {}) should={} actual={} rem={}".format(
        "OK    " if actual == expected else "NOT OK",
        num,
        denom,
        expected,
        actual,
        rem,
    ))
