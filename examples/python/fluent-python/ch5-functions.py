import random
import bobo
import inspect
from inspect import signature


def factorial(n):
    """Returns the factorial of n: aka n!"""
    return 1 if n < 2 else n * factorial(n-1)


print("factorial(42): {}".format(factorial(42)))
print("factorial.__doc__: {}".format(factorial.__doc__))

res = map(factorial, range(11))
print("res: {}".format(list(res)))

fruits = ['strawberry', 'fig', 'apple', 'cherry', 'raspberry', 'banana']

f2 = sorted(fruits, key=len)
print("f2: {}".format(f2))

def reverse(word):
    return word[::-1]

print("reverse(tree): {}".format(reverse('tree')))

f3 = sorted(fruits, key=reverse)
print("f3: {}".format(f3))


# Q: does using a key fn have an optimization that it's only called once per item?
# eg: here's the "schwartzian transform" from Perl that avoids duplicat calls to the
# keyfn by caching at the cost of extra memory
def mktuple(elt):
    return (elt[1:], elt)

f4 = list(map(lambda e: e[1], sorted(map(mktuple, fruits))))
print("f4: {}".format(f4))


TIMESCALLED = {}

def getkey(elt):
    global TIMESCALLED
    TIMESCALLED[elt] = 1 + TIMESCALLED.get(elt, 0)
    return elt[1:]

def lotsawords():
    res = []
    with open('/usr/share/dict/words', 'r') as fh:
        for _ in range(10000):
            res.append(fh.readline())
    return res

words = lotsawords()

f5 = sorted(words, key=getkey)
# print("TIMESCALLED: {}".format(TIMESCALLED))
for k, v in TIMESCALLED.items():
    if v < 2:
        continue
    print("k,v: {}".format((k,v)))

# NB: great, there are none!

class BingoCage:
    def __init__(self, items):
        self._items = list(items)
        random.shuffle(self._items)

    def pick(self):
        try:
            return self._items.pop()
        except IndexError:
            raise LookupError("Nothing left, the BingoCage is empty")

    def __call__(self):
        return self.pick()


bingo = BingoCage(range(5))
print("bingo.pick(): {}".format(bingo.pick()))
print("bingo(): {}".format(bingo()))

print("callable(bingo): {}".format(callable(bingo)))


class C:
    pass


obj = C()
def func():
    pass

funcs = sorted(set(dir(func)) - set(dir(obj)))
print("funcs: {}".format(funcs))




def tag(name, *content, cls=None, **attrs):
# def tag(name, *content, **attrs):
    # cls = attrs.get('cls', None)
    # attrs.pop('cls', None)
    if cls is not None:
        attrs['class'] = cls
    if attrs:
        attr_str = ''.join(' %s="%s"' % (attr, value) 
                           for attr, value
                           in sorted(attrs.items()))
    else:
        attr_str = ''

    if content:
        return '\n'.join('<%s%s>%s</%s>' % (name, attr_str, c, name) for c in content)
    else:
        return '<%s%s />' % (name, attr_str)

print("tag('br'): {}".format(tag('br')))
print("tag('p', 'hello'): {}".format(tag('p', 'hello')))
print("tag('p', 'hello', 'world'): {}".format(tag('p', 'hello', 'world')))
print("tag('p', 'hello', id=3): {}".format(tag('p', 'hello', id=3)))
print("tag('p', 'hello', 'world', cls='sidebar'): {}".format(tag('p', 'hello', 'world', cls='sidebar')))


my_tag = {'name': 'img', 'title': 'Sunset Boulevard', 'src': 'sunset.jpg', 'cls': 'framed'}
print("tag(**my_tag): {}".format(tag(**my_tag)))


@bobo.query('/')
def hello(person):
    return 'Hello %s!' % person

print("Run with bobo -f ch5-functions.py")
print("curl -i http://localhost:8080/?person=Bill")


def clip(text, max_len=80):
    end = None
    if len(text) > max_len:
        space_before = text.rfind(' ', 0, max_len)
        if space_before >= 0:
            end = space_before
        else:
            space_after = text.rfind(' ', max_len)
            if space_after >= 0:
                end = space_after
    if end is None: # no spaces found
        end = len(text)
    return text[:end].rstrip()

print("clip.__defaults__: {}".format(clip.__defaults__))
print("clip.__code__: {}".format(clip.__code__))
print("clip.__code__.co_varnames: {}".format(clip.__code__.co_varnames))
print("clip.__code__.co_argcount: {}".format(clip.__code__.co_argcount))

print("signature(clip): {}".format(signature(clip)))
sig = signature(clip)
for name, param in sig.parameters.items():
    print("param.kind:name=default: {}:{}={}".format(
        param.kind, name, param.default
    ))
    
sig = inspect.signature(tag)
my_tag = {'name': 'img', 'title': 'Sunset Boulevard', 'src': 'sunset.jpg', 'cls': 'framed'}
bound_args = sig.bind(**my_tag)
print("bound_args: {}".format(bound_args))
for name, value in bound_args.arguments.items():
    print("name=value: {}={}".format(name, value))


def clip(text:str, max_len:'int > 0'=80) -> str:
    end = None
    if len(text) > max_len:
        space_before = text.rfind(' ', 0, max_len)
        if space_before >= 0:
            end = space_before
        else:
            space_after = text.rfind(' ', max_len)
            if space_after >= 0:
                end = space_after
    if end is None: # no spaces found
        end = len(text)
    return text[:end].rstrip()
    
print("clip.__annotations__: {}".format(clip.__annotations__))

sig = signature(clip)
print("sig.return_annotation: {}".format(sig.return_annotation))
for param in sig.parameters.values():
    note = repr(param.annotation).ljust(13)
    print("note:name=default: {}:{}={}".format(note, param.name, param.default))
