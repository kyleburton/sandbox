package main

import (
  "flag"
  "fmt"
  "stevenson"
)

func main () {
  configPath := flag.String("config", ".stevenson.yaml", "Override the default configuration file path.")
  autoMode   := flag.Bool("auto", false, "Watch the source directory and auto-regenerate the site on any changes.")
  flag.Parse()

  fmt.Printf("Stevenson, GO!\n")
  stevenson.LoadConfiguration(configPath)
  stevenson.Configuration.AutoMode = *autoMode
  fmt.Printf("Using configuration: %q\n", stevenson.Configuration)
  stevenson.Run()
  fmt.Printf("I bring it to and end.\n")
}
