package main

import (
	"encoding/json"
	"strings"
	"fmt"
	"io"
)

type Message struct {
  Field1 string
  Field2 []float64
}

func (self *Message) String() string {
  return fmt.Sprintf("{\"Field1\": %s, \"Field2\": %q}", self.Field1, self.Field2)
}


func main() {
  const jsonStr = `
{"Field1": "that", "Field2": [1, 3, 3.4]}
`

  dec := json.NewDecoder(strings.NewReader(jsonStr))
  for {
    var m Message
    if err := dec.Decode(&m); err == io.EOF {
      break
    } else if err != nil {
      panic(err)
    }
    fmt.Printf("Decoded: %q\n", &m)
  }

  const jsonStr2 = `
{"this": "that", "other": [1, 3, 3.4], "nested": {"thing": ["five", 3]}, "moreStuff": {"andMore": [{"a": 1, "b": {"c": 3}}]}}
`
  stuff := make(map[string] interface{})

  err := json.Unmarshal([]byte(jsonStr2), &stuff)
  if err != nil {
    panic(err)
  }

  fmt.Printf("stuff=%q\n", stuff)
}

