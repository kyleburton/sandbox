class Node(object):
    def __init__(self, v, nxt):
        self.value = v
        self.next = nxt

    @classmethod
    def new(self, *elts):
        lst = None
        for elt in elts:
            lst = Node(elt, lst)
        return lst

    def to_array(self):
        ary = []
        lst = self
        while lst is not None:
            ary.append(lst.value)
            lst = lst.next
        return ary

    def __repr__(self):
        return "Node.0x{}{{{}}}".format(
            "0x%x" % (id(self)),
            ", ".join([str(x) for x in self.to_array()])
        )

    def len(self):
        l = 0
        lst = self
        while lst is not None:
            l = l + 1
            lst = lst.next
        return l

    def end(self):
        lst = self
        prev = None
        while lst is not None:
            prev = lst
            lst = lst.next
        return prev

    def mid(self):
        tortise = self
        hare = self
        while hare is not None:
            hare = hare.next
            if hare is None:
                break
            hare = hare.next
            tortise = tortise.next
        return tortise

l1 = Node.new(1, 2, 3, 4, 5)
print("l1:            {}".format(l1))
print("l1:            {}".format(l1.to_array()))
print("l1.len()       {}".format(l1.len()))
print("l1.end()       {}".format(l1.end()))

# Print the Middle of a given linked list
print("l1.mid():      {}".format(l1.mid()))

# Flattening a linked list
# Delete the elements in an linked list whose sum is equal to zero
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
