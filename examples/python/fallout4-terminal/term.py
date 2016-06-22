import sys
from random import shuffle, choice, random # NOQA

words_file = "words.txt"


def slurp_lines(fname):
    with open(fname, 'r') as f:
        return [l.rstrip().upper()
                for l in f.readlines()
                if l.rstrip() == l.rstrip().lower()]


def chunk(l, n):
    for i in range(0, len(l), n):
        yield l[i:i+n]


def chars_in_common(w1, w2):
    positions = []
    num_in_common = 0
    for idx in range(len(w1)):
        if w1[idx] == w2[idx]:
            num_in_common = num_in_common + 1
            positions.append('1')
        else:
            positions.append('0')
    return (num_in_common, "".join(positions))


def get_decoys(target_word, max_words=40):
    if len(target_word) <= 4:
        raise Exception("Error: word is too short, must " +
                        "have a len of at least 4")
    decoys = []
    # TODO: make sure the words we're testing have at least 1 character in
    # common w/the target word
    for decoy in slurp_lines(words_file):
        if len(target_word) == len(decoy) and target_word != decoy:
            decoys.append(decoy)

    # we want a distribution of matches:
    # 2 or more with 0 chars in common
    # 2 or more with 1 chars in common
    # 2 or more with 2 chars in common
    # 2 or more with 3 chars in common
    shuffle(decoys)
    matched = []
    for w in decoys:
        (num_in_common, positions) = chars_in_common(target_word, w)
        while num_in_common >= len(matched):
            matched.append([])
        matched[num_in_common].append(w)

    # for idx in range(len(matched)):
    #     print("len={} {}".format(idx, ", ".join(matched[idx])))

    # NB: just assuming that we have enough words to satisfy
    d2 = []
    d2.append(matched[3].pop())
    d2.append(matched[3].pop())
    d2.append(matched[2].pop())
    d2.append(matched[2].pop())
    d2.append(matched[1].pop())
    d2.append(matched[1].pop())
    d2.append(matched[0].pop())
    d2.append(matched[0].pop())

    while len(d2) < max_words - 1:
        w = decoys.pop()
        if w in d2:
            continue
        d2.append(w)

    d2.append(target_word)
    shuffle(d2)
    return d2


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
        line = line + "   "
        line = line + "0x%04X" % (right_hex) # NOQA
        line = line + " "
        line = line + right_col[ii:ii+width]
        right_hex = right_hex + width
        print("  " + line)


def get_wordlist(target_word):
    # we want the words totake up about 70 characters
    # of the terminal
    max_words = int(70 / len(target_word)) + 1
    return get_decoys(target_word, max_words)


def generate_terminal(target_word, specs=(12, 14)):
    (width, height) = specs
    total_len = width * height * 2
    decoys = get_wordlist(target_word)
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


def test_match(target, candidate):
    if len(target) != len(candidate):
        raise Exception("Error: length's don't match!")
    (num_in_common, positions) = chars_in_common(target, candidate)
    matched = len(target) == num_in_common
    return (num_in_common, positions, matched)


if __name__ == "__main__":
    for word in sys.argv[1:]:
        generate_terminal(word.upper())
