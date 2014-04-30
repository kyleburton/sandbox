package main

import (
  "fmt"
  "time"
)

func main () {
  c1 := make(chan int)
  fmt.Printf("select: c1=%s\n", c1)

  c2 := make(chan int)

  quit := make(chan bool)

  go func () {
    var v1, v2 int
    for ii := 0; ii < 1000; ii++ {
      time.Sleep(10)
      select {
        case v1 = <-c1:
          fmt.Printf("from c1, got: %d\n", v1)
        case v2 = <-c2:
            fmt.Printf("from c2, got: %d\n", v2)
        default:
              fmt.Printf("nothing\n")
      }
    }
    quit <- true
  }()

  for ii := 0; ii < 10; ii++ {
    time.Sleep(100)
    if ii % 2 == 1 {
      c1 <- ii
    } else {
      c2 <- ii
    }
  }

  <-quit

  fmt.Printf("all done\n")
}
