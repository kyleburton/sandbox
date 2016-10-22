from term import get_wordlist, test_match

target_word = 'hospital'.upper()

words = get_wordlist(target_word)

print("Find the match in: {}".format(words))

attempts = words[:]
candidate = attempts.pop()

while True:
    (num_in_common, positions, matched) = test_match(target_word, candidate)
    print("%s: %-5s -- %s / %s" % (candidate,
                                   matched,
                                   num_in_common,
                                   positions))
    if not attempts:
        break
    candidate = attempts.pop()


# pick a random candidate that meets the following criteria:
#   anywhere a previous candidate had a 0, it has a distinct character
#   anywhere a previous candidate had a 1, it has the same character
# test it, keep track of the count and position of matches
# if matched, great!
# if not matched find a
def find_candidates(attempts, words):
    pass

attempts = []
while words:
    # NB: this pops words
    candidate = find_candidates(attempts, words)
    (num_in_common, positions, matched) = test_match(target_word, candidate)
    attempts.append((candidate, num_in_common, positions, matched))
