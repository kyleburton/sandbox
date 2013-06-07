package main

import (
  "os/exec"
  "log"
  "fmt"
)

func main () {
  out, err := exec.Command("date").Output()
  if err != nil {
    log.Fatal(err)
  }
  fmt.Printf("The date is %s\n", out)
}
