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


def main():
    print("in main()")
    print("registry: {}".format(registry))
    f1()
    f2()
    f3()


if __name__ == '__main__':
    main()
