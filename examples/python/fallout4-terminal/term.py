import sys
from random import shuffle, choice, random # NOQA

words_file = "words.txt"


def slurp_lines(fname):
    with open(fname, 'r') as f:
        return [l.rstrip().upper() for l in f.readlines()]


def chunk(l, n):
    for i in range(0, len(l), n):
        yield l[i:i+n]


def get_decoys(target_word, max=20):
    decoys = []
    for decoy in slurp_lines(words_file):
        if len(target_word) == len(decoy) and target_word != decoy:
            decoys.append(decoy)
    shuffle(decoys)
    return decoys[0:max]


def print_terminal(term, specs=(12, 14)):
    (width, height) = specs
    midpoint = width * height
    left_col, right_col = (term[0:midpoint], term[midpoint+1:])
    left_hex = int(random() * 1024)
    right_hex = left_hex + midpoint
    for ii in range(0, midpoint, width):
        line =        "0x%04X" % (left_hex) # NOQA
        line = line + " "
        line = line + left_col[ii:ii+width]
        left_hex = left_hex + width
        line = line + " "
        line = line + "0x%04X" % (right_hex) # NOQA
        line = line + " "
        line = line + right_col[ii:ii+width]
        right_hex = right_hex + width
        print("  " + line)


def generate_terminal(target_word, specs=(12, 14)):
    (width, height) = specs
    total_len = width * height * 2
    decoys = get_decoys(target_word, 12)
    decoys.append(target_word)
    shuffle(decoys)
    # print("{} decoys found for {}".format(len(decoys), target_word))
    # for decoy in decoys:
    #     print("{}: {}".format(target_word, decoy))

    # a termianl is two columns of 12 wrapped charactrs
    # of 16 lines.  There are roughly 14 words (when len=5)

    # place all of our words in the string surrounded by spaces
    # insert random amounts of extra space within the string to
    # push the words away from each other
    avg_dist = int(total_len / (len(decoys) + 2)) + 1
    term = (" " * avg_dist)
    while decoys:
        term = term + (" " * avg_dist) + decoys.pop()
    term = term + (" " * avg_dist)

    # randomly remove spaces till we're == total_len
    while len(term) > total_len:
        # find a space & remove it
        pos = int(random() * len(term))
        if term[pos] == ' ':
            term = term[0:pos] + term[pos+1:]

    # replace the whitespace with random characters
    rand_chars = [c for c in "{}[]|\/.'`%=_><$:!@#;+*)(^"]
    new_term = ""
    for ch in term:
        if ' ' == ch:
            new_term = new_term + choice(rand_chars)
        else:
            new_term = new_term + ch

    print_terminal(new_term)


for word in sys.argv[1:]:
    generate_terminal(word.upper())
