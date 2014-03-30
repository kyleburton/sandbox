package main

import(
  "fmt"
  "flag"
)


type Route struct {
  Pattern []string
  Params map[string]string
}


var RoutingTable []*Route

func InitRoutingTable () {
  RoutingTable = make([]*Route, 0)
  RoutingTable = append(RoutingTable, &Route {
    Pattern: []string{"types", "ls",},
    Params:  make(map[string]string),
  })
}

func RouteMatches( args []string) bool {
  return false
}

func FindMatchingRoute(args []string) *Route {
  return nil
}

func main () {
  InitRoutingTable()
  flag.Parse()
  route := FindMatchingRoute(flag.Args())
  fmt.Printf("Args: %s\n", flag.Args())
  fmt.Printf("Route: %s\n", route);
}
