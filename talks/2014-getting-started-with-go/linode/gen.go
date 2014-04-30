package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	_ "os"
	"strings"
	"unicode"
	"sort"
)

type ApiSpecParameter struct {
  // Linode is nice enough to give us a machine readable spec, but
  // they are inconsistent with respect to the Required field in Parameters, it
  // is sometimes a boolean (true|false), and sometimes a string ("yes")
	Required    interface{}
	Type        string
	Description string
	Name        string
}

type ApiSpecMethod struct {
	Throws      string
	Parameters  map[string]ApiSpecParameter
	Description string
}

type ApiSpecMethodsList map[string]ApiSpecMethod

type ApiSpecMethods struct {
  Methods ApiSpecMethodsList
  Version float64
}

type ApiSpecResponse struct {
	Action     string
	Data       ApiSpecMethods
	Errorarray []float64
}

func UcFirst (s string) string {
  a := []rune(s)
  a[0] = unicode.ToUpper(a[0])
  return string(a)
}

func MethodNameToStructName (s string) string {
  var res string
  parts := strings.Split(s, ".") 
  for _, part := range parts {
    res += UcFirst(part)
  }
  return res
}

func LinodeParamTypeToGoType (s string) string {
	switch s {
	case "numeric":
    return "float"
	case "string":
    return "string"
	case "boolean":
    return "bool"
  default:
    return "string"
  }
}

type ByString []string

func (a ByString) Len() int           { return len(a) }
func (a ByString) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByString) Less(i, j int) bool { return a[i] < a[j] }

func StringMapKeys (m map[string]interface{}) []string {
  res := make([]string, 0)
  for k, _ := range m {
    res = append(res, k)
  }
  return res
}

func main() {
	//fmt.Fprintf(os.Stderr, "Loading...\n")

	file, err := ioutil.ReadFile("api.spec.json")
	if err != nil {
		panic(err)
	}

	var spec ApiSpecResponse
	err = json.Unmarshal(file, &spec)
	if err != nil {
		panic(err)
	}

  methodNames := make([]string, 0)
  for k, _ := range spec.Data.Methods {
    methodNames = append(methodNames, k)
  }
	sort.Sort(ByString(methodNames))
  // for name, method := range spec.Data.Methods {
  fmt.Printf("package linode\n\n")
  for _, name := range methodNames {
    method := spec.Data.Methods[name]
    //fmt.Fprintf(os.Stderr, "%s=%s\n", name, method)
    fmt.Printf("// %s\n", method.Description)
    fmt.Printf("type %s struct {\n", MethodNameToStructName(name))
    fmt.Printf("	Command string `%s`\n", name)
    fmt.Printf("	Throws string `%s`\n", method.Throws)
    paramNames := make([]string, 0)
    for k, _ := range method.Parameters {
      paramNames = append(paramNames, k)
    }
    sort.Sort(ByString(paramNames))
    // for pname, parameter := range method.Parameters {
    for _, pname := range paramNames {
      parameter := method.Parameters[pname]
      fmt.Printf("	%s %s // %s\n", UcFirst(pname), LinodeParamTypeToGoType(parameter.Type), parameter.Description)
    }
    fmt.Printf("}\n\n\n")
  }
}
