class Node():
    def __init__(self, left=None, val=None, right=None):
        self.left = left
        self.value = val
        self.right = right


def tree_new():
    return None

def tree_add(t, val):
    # print("tree_add(t, val={})".format(val))
    if t is None:
        return Node(None, val, None)
    if val < t.value:
        return Node(tree_add(t.left, val), t.value, t.right)
    if val == t.value:
        return t
    if val > t.value:
        return Node(t.left, t.value, tree_add(t.right, val))

def pre_order(t, f):
    if t is None:
        return
    pre_order(t.left, f)
    f(t.value)
    pre_order(t.right, f)

def post_order(t, f):
    if t is None:
        return
    post_order(t.right, f)
    f(t.value)
    post_order(t.left, f)

def printme_pre_order(t):
  def visitor(val):
      print("Node{{val={}}}".format(val))
  pre_order(t, visitor)

def printme_post_order(t):
  def visitor(val):
      print("Node{{val={}}}".format(val))
  post_order(t, visitor)

# O(N) b/c we don't take advantage of the left/right
def find_slow(t, v):
    if t is None:
        return False

    nodes = [t]
    while nodes:
        node = nodes.pop()
        if node.value == v:
            return True
        if node.left is not None:
            nodes.append(node.left)
        if node.right is not None:
            nodes.append(node.right)
    return False

# O(ln(N))
def find_smart(t, v):
    if t is None:
        return False

    node = t
    while node:
        if node.value == v:
            return True
        if v < node.value:
            node = node.left
            continue
        if v > node.value:
            node = node.right
            continue
    return False

# O(ln(N))
def find_smart_recursive(t, v):
    if t is None:
        return False

    if t.value == v:
        return True
    if v < t.value:
        return find_smart_recursive(t.left, v)
    if v > t.value:
        return find_smart_recursive(t.right, v)
    return False


produce = ["orange", "lettuce", "cucumber", "carrots", "pear", "watermellon"]
t = tree_new()
for p in produce:
    t = tree_add(t, p)

print("pre:")
printme_pre_order(t)
print("post:")
printme_post_order(t)

print("find_slow(\"apple\") => {}", find_slow(t, "apple"))
print("find_slow(\"orange\") => {}", find_slow(t, "orange"))

print("find_smart(\"apple\") => {}", find_smart(t, "apple"))
print("find_smart(\"orange\") => {}", find_smart(t, "orange"))
