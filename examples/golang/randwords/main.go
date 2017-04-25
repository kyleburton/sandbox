package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"math/rand"
	"os"
	"time"
	"unicode"
)

// https://en.wikipedia.org/wiki/Reservoir_sampling
func main() {
	var rnd = rand.New(rand.NewSource(time.Now().UnixNano()))
	var wordsFile string
	var numWords int
	var includeProperNouns bool

	flag.StringVar(&wordsFile, "wordsFile", "/usr/share/dict/words", "The path to your system's words file.")
	flag.IntVar(&numWords, "numWords", 5, "The number of words to choose.")
	flag.BoolVar(&includeProperNouns, "includeProperNouns", false, "Include proper nouns (capitalized words)")
	// TODO: minLen "Specify a minimum word length"
	// TODO: maxLen "Specify a maximum word length"
	flag.Parse()

	file, err := os.Open(wordsFile)
	if err != nil {
		log.Fatal(fmt.Sprintf("Error opening file: %s : %s", wordsFile, err))
		os.Exit(1)
	}
	defer file.Close()

	resevior := make([]string, numWords)

	// TODO: this could be a general library if we define it in terms of channels:
	//   reseviorSample(reseviorSlice[]interface{}, in chan(interface{}), out chan(interface{}))
	//      reseviorSlice: the resevior, len is the # of samples
	//      in:            input channel of items
	//      out:           a channel that will emit 1 item, the resul when in is exhausted
	scanner := bufio.NewScanner(file)

	var ii = 0
	var reseviorLength = len(resevior)

	// first fill the resevior with the first numWords words from the file
	for scanner.Scan() && ii < reseviorLength {
		line := scanner.Text()
		resevior[ii] = line
		ii = ii + 1
	}

	// for the rest of the words in the file, replace with decreasing
	// probability
	for scanner.Scan() {
		line := scanner.Text()
		if !includeProperNouns && unicode.IsUpper(([]rune(line))[0]) {
			continue
		}
		jj := rnd.Intn(ii)
		if jj < reseviorLength {
			resevior[jj] = line
		}
		ii = ii + 1
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
		os.Exit(1)
	}

	for idx, word := range resevior {
		if idx >= ii {
			break
		}
		fmt.Printf("%s\n", word)
	}

	os.Exit(0)
}
