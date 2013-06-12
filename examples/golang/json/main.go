package main

import (
	"encoding/json"
	"strings"
	"fmt"
	"io"
)

func main() {
  const jsonStr = `
{"Field1": "that", "Field2": [1, 3, 3.4]}
`

  type Message struct {
    Field1 string
    Field2 []float64
  }

  dec := json.NewDecoder(strings.NewReader(jsonStr))
  for {
    var m Message
    if err := dec.Decode(&m); err == io.EOF {
      break
    } else if err != nil {
      panic(err)
    }
    fmt.Printf("Decoded: %q\n", m)
  }

  const jsonStr2 = `
{"this": "that", "other": [1, 3, 3.4], "nested": {"thing": ["five", 3]}}
`
  stuff := make(map[string] interface{})

  err := json.Unmarshal([]byte(jsonStr2), &stuff)
  if err != nil {
    panic(err)
  }

  fmt.Printf("stuff=%q\n", stuff)
}

