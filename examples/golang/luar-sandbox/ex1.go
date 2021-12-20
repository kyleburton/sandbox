package main

import (
  "fmt"
  _ "os"
  "github.com/stevedonovan/luar"
)

func MyFunc (s string, ii int) {
  fmt.Printf("%s: %v\n", s, ii)
}

func main () {

  luaCode := `
for i = 1,10 do
  MyFunc(MSG,i)
end
  `

  L := luar.Init()
  defer L.Close()

  luar.Register(L, "", luar.Map {
    //"MyFunc":  fmt.Println,
    "MyFunc":  MyFunc,
    "MSG":     "Hello, Luar!",
  })

  err := L.DoString(luaCode)
  if err != nil {
    panic(err)
  }
}
