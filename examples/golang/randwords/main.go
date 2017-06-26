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

func NewLineChannel(fname string) (chan(string), error) {
  f, err := os.Open(fname)
  if err != nil {
    return nil, err
  }


  c := make(chan(string))

  go func () {
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
      c <- scanner.Text()
    }
    f.Close()
    close(c)
  }()

  return c, nil
}

// TODO: urfave/cli - turn this into a cli app
// TODO: allow the user to specify the # of words from the cli
// TODO: allow the user to specify the file to use, default to /usr/share/dict/words if it exists, can we embed a words list?
// TODO: filter out Capitolized words
// TODO: filter out symbols & words that arew too short (support minlen?)
func main () {
  numWords := 10
  lnum := 0
  resevior := make([]string, numWords)

  rand.Seed(time.Now().UTC().UnixNano())

  c, err := NewLineChannel("/usr/share/dict/words")
  if err != nil {
    panic(err)
  }

  for ii := 0; ii < numWords; ii++ {
    resevior[ii] = <-c
    lnum++
  }

  for line := range(c) {
    lnum++
    idx := rand.Intn(lnum)
    if idx < numWords {
      resevior[idx] = line
    }
  }

  for _, line := range(resevior) {
    fmt.Printf("%s\n", line)
  }
}

