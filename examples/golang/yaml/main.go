package main

import (
	"launchpad.net/goyaml"
	"flag"
	"fmt"
	"io/ioutil"
)

func parse_yaml( file string ) map[interface{}]interface{} {
  m := make(map[interface{}]interface{})

  data, err := ioutil.ReadFile(file)
  if err != nil {
    panic(fmt.Sprintf("Error reading file %s : %v", file, err))
  }

  goyaml.Unmarshal([]byte(data), &m)
  
  return m
}

func main() {
	flag.Parse()

  if len(flag.Args()) == 0 {
    panic("You must supply one or more yaml files to parse")
  }

	for _, arg := range flag.Args() {
	  fmt.Printf("arg: %v\n", arg)
    res := parse_yaml(arg)
    fmt.Printf(" parsed: %v\n", res)

    yml, err := goyaml.Marshal(res)
    if err != nil {
      panic(err);
    }
    fmt.Printf(" back to yaml: %v\n", string(yml))
  }
}
