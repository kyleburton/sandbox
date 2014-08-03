package main

import (
  _ "fmt"
  "strconv"
  _ "os"
  "github.com/stevedonovan/luar"
)

func GoFun (args []int) (res map[string]int) {
  res = make(map[string]int)

  for i,val := range args {
    res[strconv.Itoa(i)] = val * val
  }
  return
}

func main () {

  luaCode := `
print 'in the lua code'
local res = GoFun {10, 20, 30, 40}
print(res['1'], res['2'])
res = luar.map2table(res)
for k,v in pairs(res) do
  print(k,v)
end
  `

  L := luar.Init()
  defer L.Close()

  luar.Register(L, "", luar.Map {
    "GoFun":  GoFun,
  })

  err := L.DoString(luaCode)
  if err != nil {
    panic(err)
  }
}

