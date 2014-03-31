package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"strings"
	"os"
	"net/http"
	"errors"
)

type ConfigType map[string]string
var Config ConfigType

var Verbose bool = false

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
    Handler: DoSizesLs,
	})

	RoutingTable = append(RoutingTable, &Route{
    Pattern: []string{"droplets", "ls", ":dropletId"},
		Params:  make(map[string]string),
    Handler: DoDropletsLsDroplet,
	})

	RoutingTable = append(RoutingTable, &Route{
    Pattern: []string{"droplets", "destroy", ":dropletId"},
		Params:  make(map[string]string),
    Handler: DoDropletsDestroyDroplet,
	})

//	RoutingTable = append(RoutingTable, &Route{
//    Pattern: []string{"droplets", "new", ":name", ":size", ":image", ":region", ":private_networking", ":backups_enabled"},
//		Params:  make(map[string]string),
//    Handler: DoDropletsNewDroplet,
//	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"droplets", "ls"},
		Params:  make(map[string]string),
    Handler: DoDropletsLs,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"images", "ls"},
		Params:  make(map[string]string),
    Handler: DoImagesLs,
	})

	RoutingTable = append(RoutingTable, &Route{
    Pattern: []string{"images", "show", ":imageId"},
		Params:  make(map[string]string),
    Handler: DoImageShow,
	})

}

func ApiGet (path string) (*http.Response, map[string]interface{}, error) {
  url := fmt.Sprintf("https://api.digitalocean.com%s?client_id=%s&api_key=%s", path, Config["ClientId"], Config["ApiKey"])
  if Verbose {
    fmt.Fprintf(os.Stderr, "ApiGet: url=%s\n", url)
  }
  resp, err := http.Get(url)
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error[GET:%s]: %s\n", url, err)
    return resp, nil, nil
  }

  defer resp.Body.Close()
  body, err := ioutil.ReadAll(resp.Body)
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error: %s\n", err)
    return resp, nil, err
  }

  var content map[string]interface{}
  json.Unmarshal(body, &content)

  if Verbose {
    fmt.Fprintf(os.Stderr, "  resp: %s\n", resp)
  }
  if Verbose {
    fmt.Fprintf(os.Stderr, "  content: %s\n", content)
  }

  if content["status"] != "OK" {
    return resp, content, errors.New(fmt.Sprintf("Request Failed (not OK): status=%s", content["status"]))
  }

  return resp, content, nil
}

func MapGetString (m map[string]interface{}, k string, defaultValue string) string {
  val, ok := m[k]
  if ok && val != nil {
    return val.(string)
  }

  return defaultValue
}

func DoImagesLs (route *Route) {
  _, content, err := ApiGet("/images/")

  if err != nil {
    fmt.Fprintf(os.Stderr, "Error: %s\n", err)
    os.Exit(1)
  }

  header := []string {
    "image.id",
    "image.name",
    "image.distribution",
    "image.slug",
    "image.public",
  }

  fmt.Printf("%s\n", strings.Join(header, "\t"))
  elements := content["images"].([]interface{})
  for _, elt := range elements {
    //fmt.Printf("elt:%s\n", elt)
    item := elt.(map[string]interface{})
    fmt.Print(strings.Join([]string{
      fmt.Sprintf("%.f", item["id"]),
      MapGetString(item, "name", ""),
      MapGetString(item, "distribution", ""),
      MapGetString(item, "slug", ""),
      fmt.Sprintf("%t", item["public"]),
    }, "\t"))
    fmt.Print("\n")
  }
}

func DoImageShow (route *Route) {
  _, content, err := ApiGet(fmt.Sprintf("/images/%s", route.Params["imageId"]))

  if err != nil {
    fmt.Fprintf(os.Stderr, "Error: %s\n", err)
    os.Exit(1)
  }

  header := []string {
    "image.id",
    "image.name",
    "image.distribution",
  }

  fmt.Printf("%s\n", strings.Join(header, "\t"))
  item := content["image"].(map[string]interface{})
  if Verbose {
    fmt.Fprintf(os.Stderr, "item: %s\n", item)
  }
  fmt.Print(strings.Join([]string{
    fmt.Sprintf("%.f", item["id"]),
    MapGetString(item, "name", ""),
    MapGetString(item, "distribution", ""),
  }, "\t"))
  fmt.Print("\n")
}

func DoDropletsLs (route *Route) {
  //response, body, err := ApiGet("/sizes/")
  _, content, err := ApiGet("/droplets/")

  if err != nil {
    fmt.Fprintf(os.Stderr, "Error: %s\n", err)
    os.Exit(1)
  }

  header := []string {
    "droplet.id",
    "droplet.name",
    "droplet.image_id",
    "droplet.size_id",
    "droplet.region_id",
    "droplet.backups_active",
    "droplet.ip_address",
    "droplet.private_ip_address",
    "droplet.locked",
    "droplet.status",
    "droplet.created_at",
  }

  fmt.Printf("%s\n", strings.Join(header, "\t"))
  elements := content["droplets"].([]interface{})
  for _, elt := range elements {
    item := elt.(map[string]interface{})
    fmt.Print(strings.Join([]string{
      fmt.Sprintf("%.f", item["id"]),
      item["name"].(string),
      fmt.Sprintf("%.f", item["image_id"]),
      fmt.Sprintf("%.f", item["size_id"]),
      fmt.Sprintf("%.f", item["region_id"]),
      item["backups_active"].(string),
      item["ip_address"].(string),
      item["private_ip_address"].(string),
      item["locked"].(string),
      item["status"].(string),
      item["created_at"].(string),
    }, "\t"))
    fmt.Print("\n")
    // , id, size["name"], size["slug"])
  }
}

func DoDropletsLsDroplet (route *Route) {
  if Verbose {
    fmt.Fprintf(os.Stderr, "DoDropletsLsDroplet %s\n", route)
  }
  _, content, err := ApiGet(fmt.Sprintf("/droplets/%s", route.Params["dropletId"]))

  if err != nil {
    fmt.Fprintf(os.Stderr, "Error: %s\n", err)
    os.Exit(1)
  }

  header := []string {
    "droplet.id",
    "droplet.name",
    "droplet.image_id",
    "droplet.size_id",
    "droplet.region_id",
    "droplet.backups_active",
    "droplet.backups",
    "droplet.snapshots",
    "droplet.ip_address",
    "droplet.private_ip_address",
    "droplet.locked",
    "droplet.status",
  }

  fmt.Printf("%s\n", strings.Join(header, "\t"))
  item := content["droplet"].(map[string]interface{})
  fmt.Print(strings.Join([]string{
    fmt.Sprintf("%.f", item["id"]),
    item["name"].(string),
    fmt.Sprintf("%.f", item["image_id"]),
    fmt.Sprintf("%.f", item["size_id"]),
    fmt.Sprintf("%.f", item["region_id"]),
    item["backups_active"].(string),
    // TODO: join these on a comma or something
    item["backups"].(string),
    // TODO: join these on a comma or something
    item["snapshots"].(string),
    item["ip_address"].(string),
    item["private_ip_address"].(string),
    item["locked"].(string),
    item["status"].(string),
  }, "\t"))
  fmt.Print("\n")
}

func DoDropletsDestroyDroplet (route *Route) {
  if Verbose {
    fmt.Fprintf(os.Stderr, "DoDropletsDestroyDroplet %s\n", route)
  }
  _, content, err := ApiGet(fmt.Sprintf("/droplets/%s/destroy/", route.Params["dropletId"]))

  if err != nil {
    fmt.Fprintf(os.Stderr, "Error: %s\n", err)
    os.Exit(1)
  }

  fmt.Printf("event_id=%.f\n", content["event_id"])
}

func DoSizesLs (route *Route) {
  if Verbose {
    fmt.Fprintf(os.Stderr, "SizesLs: %s\n", route)
  }
  //response, body, err := ApiGet("/sizes/")
  _, content, err := ApiGet("/sizes/")
  // pretty print the results
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error: %s\n", err)
    os.Exit(1)
  }

  fmt.Printf("size.id\tsize.name\tsize.slug\n")
  sizes := content["sizes"].([]interface{})
  for _, elt := range sizes {
    size := elt.(map[string]interface{})
    id   := fmt.Sprintf("%.f", size["id"])
    fmt.Printf("%s\t%s\t%s\n", id, size["name"], size["slug"])
  }
}

func RouteMatches(route *Route, args []string) (*Route, bool) {
	if Verbose {
    fmt.Fprintf(os.Stderr, "Route: %s args: %s\n", route, args)
  }
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
		if Verbose {
      fmt.Fprintf(os.Stderr, "  part:%s arg:%s rest:%s\n", part, arg, res.Args)
    }
		if strings.HasPrefix(part, ":") {
			res.Params[part[1:]] = arg
			continue
		}

		if part == arg {
			continue
		}

    if Verbose {
      fmt.Fprintf(os.Stderr, "  ran out of parts at idx=%d, no match\n", idx)
    }
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
	if Verbose {
    fmt.Fprintf(os.Stderr, "Args: %s\n", flag.Args())
  }
	if Verbose {
    fmt.Fprintf(os.Stderr, "Route: %s\n", route)
  }

	if !InitConfig() {
    fmt.Fprintf(os.Stderr, "Invalid or Missing configuration file.\n")
    os.Exit(1)
  }
  if Verbose {
    fmt.Fprintf(os.Stderr, "Config: %s\n", Config)
  }

  if route != nil {
    if Verbose {
      fmt.Fprintf(os.Stderr, "Calling route: %s\n", route)
    }
    route.Handler(route)
  }
}
