from encodings.aliases import aliases

s = 'El Niño'
print("Codecs")

for codec in ['latin_1', 'utf_8', 'utf_16']:
    print("codec({}) {}: {}".format(codec, s, s.encode(codec)))

# NB: stackoverflow suggests this is an incomplete set, still interesting IMO
all_the_things = set()
for alias in aliases.items():
    all_the_things.add(alias[0])
    all_the_things.add(alias[1])

def chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]

for elts in chunks(list(all_the_things), 8):
    print(", ".join(elts))
