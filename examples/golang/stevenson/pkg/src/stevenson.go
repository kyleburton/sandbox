package main

import (
  "flag"
  "fmt"
  "stevenson"
)

func main () {
  configPath := flag.String("config", ".stevenson.yaml", "Override the default configuration file path.")
  flag.Parse()

  fmt.Printf("Stevenson, GO!\n")
  stevenson.LoadConfiguration(configPath)
  fmt.Printf("Using configuration: %q\n", stevenson.Configuration)
  stevenson.Run()
  fmt.Printf("I bring it to and end.\n")
}
