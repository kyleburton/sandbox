package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"strings"
	"os"
	"net/http"
)

type ConfigType map[string]string
var Config ConfigType

type RouteHandler func(*Route)

type Route struct {
	Pattern []string
	Params  map[string]string
	Args    []string
  Handler RouteHandler
}

var RoutingTable []*Route

func InitRoutingTable() {
	RoutingTable = make([]*Route, 0)

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"sizes", "ls"},
		Params:  make(map[string]string),
    Handler: SizesLs,
	})

}

func SizesLs (route *Route) {
  fmt.Printf("SizesLs: %s\n", route)
  // TODO: factor out gets into a helper function that just takes the path
  // and returns the parsed JSON body
  resp, err := http.Get(fmt.Sprintf("https://api.digitalocean.com/sizes/?client_id=%s&api_key=%s", Config["ClientId"], Config["ApiKey"]))
  if err != nil {
    fmt.Printf("Error: %s\n", err)
    os.Exit(1)
  }

  defer resp.Body.Close()
  body, err := ioutil.ReadAll(resp.Body)
  if err != nil {
    fmt.Printf("Error: %s\n", err)
    os.Exit(1)
  }

  fmt.Printf("  resp: %s\n", resp)
  fmt.Printf("  body: %s\n", body)
  // TODO: parse the body as JSON
  // check for Status=="OK"
  // pretty print the results
}

func RouteMatches(route *Route, args []string) (*Route, bool) {
	fmt.Printf("Route: %s args: %s\n", route, args)
	if len(args) < len(route.Pattern) {
		return nil, false
	}
	var res *Route = &Route{
		Pattern: route.Pattern,
		Params:  make(map[string]string),
    Handler: route.Handler,
	}

	for idx, part := range route.Pattern {
		arg := args[idx]
		res.Args = args[idx:]
		fmt.Printf("  part:%s arg:%s rest:%s\n", part, arg, res.Args)
		if strings.HasPrefix(part, ":") {
			res.Params[part[1:]] = arg
			continue
		}

		if part == arg {
			continue
		}

    fmt.Printf("  ran out of parts at idx=%d, no match\n", idx)
		return nil, false
	}

	return res, true
}

func FindMatchingRoute(args []string) *Route {
	for _, route := range RoutingTable {
		res, matched := RouteMatches(route, args)
		if matched {
			return res
		}
	}
	return nil
}

func InitConfig() bool {
	file, e := ioutil.ReadFile(os.Getenv("HOME") + "/.digitalocean.json")
	if e != nil {
		fmt.Fprintf(os.Stderr, "File error: %v\n", e)
		return false
	}

  json.Unmarshal(file, &Config)

  if _, ok := Config["ClientId"]; !ok {
		fmt.Fprintf(os.Stderr, "Error: No ClienId in configuration file!\n", e)
		return false
  }

  if _, ok := Config["ApiKey"]; !ok {
		fmt.Fprintf(os.Stderr, "Error: No ApiKey in configuration file!\n", e)
		return false
  }

  return true
}

func main() {
	InitRoutingTable()
	flag.Parse()
	route := FindMatchingRoute(flag.Args())
	fmt.Printf("Args: %s\n", flag.Args())
	fmt.Printf("Route: %s\n", route)

	if !InitConfig() {
    fmt.Fprintf(os.Stderr, "Invalid or Missing configuration file.\n")
    os.Exit(1)
  }
  fmt.Printf("Config: %s\n", Config)

  if route != nil {
    fmt.Printf("Calling route: %s\n", route)
    route.Handler(route)
  }
}
