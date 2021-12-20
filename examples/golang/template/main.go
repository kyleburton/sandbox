package main

import(
  "fmt"
  "os"
  "encoding/json"
  "io/ioutil"
  "text/template"
)

func main () {
  if len(os.Args) < 2 {
    panic(fmt.Errorf("Erorr: you must supply a template file and a json data file."))
  }

  fmt.Printf("slurp the template  from os.Args[0]: %s\n", os.Args[1])
  fmt.Printf("slurp the json data from os.Args[1]: %s\n", os.Args[2])

  tmpl, err := ioutil.ReadFile(os.Args[1])
  if nil != err {
    panic(err)
  }

  tmpl_data, err := ioutil.ReadFile(os.Args[2])
  if nil != err {
    panic(err)
  }

  var data map[string]interface{}
  err = json.Unmarshal(tmpl_data, &data)
  if nil != err {
    panic(err)
  }

  t := template.Must(template.New(os.Args[1]).Parse(string(tmpl)))

  err = t.Execute(os.Stdout, data)
  if nil != err {
    panic(err)
  }
}
