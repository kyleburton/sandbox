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
