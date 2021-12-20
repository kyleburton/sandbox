package main

import (
	"fmt"
	"os"
	"github.com/amir/raidman"
)

func main() {
	c, err := raidman.Dial("tcp", "localhost:5555")
	if err != nil {
		panic(err)
	}

  fmt.Printf("connected to localhost\n")

  hostname, err := os.Hostname()
	if err != nil {
		panic(err)
	}

	var event = &raidman.Event{
		State:   "ok",
		Host:    hostname,
		Service: "raidman-sample",
		Metric:  100,
		Ttl:     10,
	}

	err = c.Send(event)
	if err != nil {
		panic(err)
	}

  fmt.Printf("event sent\n")

	events, err := c.Query(fmt.Sprintf("host = \"%s\"", hostname))
	if err != nil {
		panic(err)
	}

	if len(events) < 1 {
		panic("Submitted event not found")
	}

  fmt.Printf("query: %q\n", events)

	c.Close()
}
