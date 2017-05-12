package main

// https://github.com/abiosoft/ishell

import (
	"fmt"
	"github.com/abiosoft/ishell"
	"strings"
)

func main() {
	shell := ishell.New()

	shell.Println("Example interactive shell")

	shell.AddCmd(&ishell.Cmd{
		Name: "greet",
		Help: "Say Hi",
		Func: func(c *ishell.Context) {
			c.Println(fmt.Sprintf("Hello, %s", strings.Join(c.Args, " ")))
		},
	})

	shell.Start()
}
