from collections import deque
dq = deque(range(10), maxlen=10)
print("dq: {}".format(dq))

dq.rotate(3)
print("dq: {}".format(dq))

dq.rotate(-4)
print("dq: {}".format(dq))

dq.appendleft(-1)
print("dq: {}".format(dq))

dq.extend([11, 22, 33])
print("dq: {}".format(dq))

dq.extendleft([10, 20, 30, 40])
print("dq: {}".format(dq))


# NB: dq.append() & dq.popleft() are atomic and thus safe when shared across threads

# see also: queue, multiprocessing, asyncio, and heapq
