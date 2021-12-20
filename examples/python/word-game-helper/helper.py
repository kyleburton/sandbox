import sys

words_file = "/usr/share/dict/words"
wlen, wchars = None, None
if len(sys.argv) >= 2:
    wlen, wchars = sys.argv[1:]

if wlen is None:
    wlen = "7"

wlen = int(wlen)

all_words = []

with open(words_file, "r") as fh:
    all_words = fh.readlines()

all_words = [x.rstrip() for x in all_words]

print("There are {} words".format(len(all_words)))
print("Finding all words of len={}".format(wlen))
len_matches = [w for w in all_words if len(w) is wlen]

#
# for w in len_matches:
#     print(" " + w)
print("Therea re {} words of len {}".format(len(len_matches), wlen))

wset = set(list(wchars))
print("finding matches for {}".format(wset))
matches = []
for w in len_matches:
    cset = set(list(w))
    if cset.issubset(wset):
        matches.append(w)


print("There are {} matches".format(len(matches)))
for w in matches:
    print(w)
