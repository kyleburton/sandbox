package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strings"
	"regexp"
)

type ConfigType map[string]string

var Config ConfigType

type CmdlineOptionsStruct struct {
	Verbose bool
}

var CmdlineOptions CmdlineOptionsStruct

// https://developers.digitalocean.com/sizes/

// GET https://api.digitalocean.com/sizes/?client_id=[client_id]&api_key=[api_key]

////////////////////

type DropletSizesResponse struct {
	Status string
	Sizes  []DropletSize
}

func (self *DropletSizesResponse) Header() []string {
	return []string{
		"id",
		"name",
		"slug",
	}
}

func (self *DropletSizesResponse) Unmarshal(content []byte) {
	json.Unmarshal(content, self)
	if self.Sizes == nil {
		self.Sizes = make([]DropletSize, 0)
	}
}

////////////////////

type DropletSize struct {
	Id   float64
	Name string
	Slug string
}

func (self *DropletSize) ToStringArray() []string {
	return []string{
		fmt.Sprintf("%.f", self.Id),
		self.Name,
		self.Slug,
	}
}

////////////////////

type ActiveDropletsResponse struct {
	Status   string
	Droplets []DropletInfo
}

func (self *ActiveDropletsResponse) Header() []string {
	return []string{
		"id",
		"name",
		"image_id",
		"size_id",
		"region_id",
		"backups_active",
		"ip_address",
		"private_ip_address",
		"locked",
		"status",
		"created_at",
	}
}

func (self *ActiveDropletsResponse) Unmarshal(content []byte) {
	json.Unmarshal(content, self)

	if self.Droplets == nil {
		self.Droplets = make([]DropletInfo, 0)
	}
}

type DropletShowResponse struct {
  Status string
  Droplet ShowDropletInfo
}

func (self *DropletShowResponse) Header() []string {
	return []string{
		"id",
		"image_id",
		"name",
		"region_id",
		"size_id",
		"backups_active",
    "backups",
		"snapshots",
		"ip_address",
		"private_ip_address",
		"locked",
		"status",
	}
}

func (self *DropletShowResponse) Unmarshal(content []byte) {
	json.Unmarshal(content, self)

  // if self.Droplet.backups == nil {
  //   self.Droplet.backups = make([]???)
  // }

  // if self.Droplet.snapshots == nil {
  //   self.Droplet.snapshots = make([]???)
  // }
}

type ShowDropletInfo struct {
	Id                 float64
	Image_id           float64
	Name               string
	Region_id          float64
	Size_id            float64
	Backups_active     bool
	Backups            []interface{}
	Snapshots          []interface{}
	Ip_address         string
	Private_ip_address string
	Locked             bool
	Status             string
}

func (self *ShowDropletInfo) ToStringArray() []string {
	return []string{
		fmt.Sprintf("%.f", self.Id),
		fmt.Sprintf("%.f", self.Image_id),
		self.Name,
		fmt.Sprintf("%.f", self.Region_id),
		fmt.Sprintf("%.f", self.Size_id),
		fmt.Sprintf("%t", self.Backups_active),
		fmt.Sprintf("%d", self.Backups),
		fmt.Sprintf("%d", self.Snapshots),
		self.Ip_address,
		self.Private_ip_address,
		fmt.Sprintf("%t", self.Locked),
		self.Status,
	}
}

type DropletInfo struct {
	Id               float64
	Name             string
	Image_id          float64
	Size_id           float64
	Region_id         float64
	Backups_active    bool
	Ip_address        string
	Private_ip_address string
	Locked           bool
	Status           string
	Created_at        string
}

func (self *DropletInfo) ToStringArray() []string {
	return []string{
		fmt.Sprintf("%.f", self.Id),
		self.Name,
		fmt.Sprintf("%.f", self.Image_id),
		fmt.Sprintf("%.f", self.Size_id),
		fmt.Sprintf("%.f", self.Region_id),
		fmt.Sprintf("%t", self.Backups_active),
		self.Ip_address,
		self.Private_ip_address,
		fmt.Sprintf("%t", self.Locked),
		self.Status,
		self.Created_at,
	}
}

////////////////////

type NewDropletResponse struct {
	Status  string
	Droplet NewDropletInfo
}

func (self *NewDropletResponse) Header() []string {
	return []string{
		"id",
		"name",
		"image_id",
		"size_id",
		"event_id",
	}
}

func (self *NewDropletResponse) Unmarshal(content []byte) {
	json.Unmarshal(content, self)
}

type NewDropletInfo struct {
	Id      float64
	Name    string
	Image_id float64
	Size_id  float64
	Event_id float64
}

func (self *NewDropletInfo) ToStringArray() []string {
	return []string{
		fmt.Sprintf("%.f", self.Id),
		self.Name,
		fmt.Sprintf("%.f", self.Image_id),
		fmt.Sprintf("%.f", self.Size_id),
		fmt.Sprintf("%.f", self.Event_id),
	}
}

////////////////////

type DestroyDropletResponse struct {
  Status string
  Event_id float64
}

func (self *DestroyDropletResponse) Unmarshal (body []byte) {
  json.Unmarshal(body, self)
}

func (self *DestroyDropletResponse) Header () []string {
  return []string {
    "event_id",
  }
}

////////////////////

type RegionResponse struct {
	Status  string
	Regions []RegionInfo
}

func (self *RegionResponse) Header() []string {
	return []string{
		"id",
		"name",
		"slug",
	}
}

func (self *RegionResponse) Unmarshal(body []byte) {
	json.Unmarshal(body, self)

	if self.Regions == nil {
		self.Regions = make([]RegionInfo, 0)
	}
}

type RegionInfo struct {
	Id   float64
	Name string
	Slug string
}

func (self *RegionInfo) ToStringArray() []string {
	return []string{
		fmt.Sprintf("%.f", self.Id),
		self.Name,
		self.Slug,
	}
}

////////////////////

type SshKeysResponse struct {
	Status   string
	Ssh_keys *[]SshKeyInfo
}

func (self *SshKeysResponse) Header() []string {
	return []string{
		"Id",
		"Name",
	}
}

func (self *SshKeysResponse) Unmarshal(body []byte) {
	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "SshKeysResponse.Unmarshal body=%s\n", body)
	}

	json.Unmarshal(body, self)
	if self.Ssh_keys == nil {
		keys := make([]SshKeyInfo, 0)
		self.Ssh_keys = &keys
	}
}

type SshKeyInfo struct {
	Id   float64
	Name string
}

func (self *SshKeyInfo) ToStringArray() []string {
	return []string{
		fmt.Sprintf("%.f", self.Id),
		self.Name,
	}
}

////////////////////

type ImagesResponse struct {
	Id     float64
	Images []ImageInfo
}

func (self *ImagesResponse) Header () []string {
  return []string {
    "id",
    "name",
    "distribution",
    "slug",
    "public",
  }
}

func (self *ImagesResponse) Unmarshal (body []byte) {
  json.Unmarshal(body, self)
}

type ImageShowResponse struct {
	Status   string
	Image ImageInfo
}

func (self *ImageShowResponse) Header () []string {
  return []string {
    "id",
    "name",
    "distribution",
    "slug",
    "public",
  }
}

func (self *ImageShowResponse) Unmarshal (body []byte) {
  json.Unmarshal(body, self)
}

type ImageInfo struct {
	Id           float64
	Name         string
	Distribution string
	Slug         string
	Public       bool
}

func (self *ImageInfo) ToStringArray() []string {
  return []string {
    fmt.Sprintf("%.f", self.Id),
    self.Name,
    self.Distribution,
    self.Slug,
    fmt.Sprintf("%t", self.Public),
  }
}

////////////////////

type EventResponse struct {
	Status   string
	Event    EventInfo
}

func (self *EventResponse) Unmarshal (body []byte) {
  json.Unmarshal(body, self)
}


func (self *EventResponse) Header () []string {
  return []string {
    "id",
    "action_status",
    "droplet_id",
    "event_type_id",
    "percentage",
  }
}

type EventInfo struct {
  Id float64
  Action_status string
  Droplet_id float64
  Event_type_id float64
  Percentage string
}

func (self *EventInfo) ToStringArray() []string {
  return []string {
    fmt.Sprintf("%.f", self.Id),
    self.Action_status,
    fmt.Sprintf("%.f", self.Droplet_id),
    fmt.Sprintf("%.f", self.Event_type_id),
    self.Percentage,
  }
}

////////////////////

type RouteHandler func(*Route)

type Route struct {
	Pattern  []string
	Params   map[string]string
	Args     []string
	Handler  RouteHandler
	HelpText *string
}

var RoutingTable []*Route

func InitRoutingTable() {
	RoutingTable = make([]*Route, 0)

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"sizes", "ls"},
		Params:  make(map[string]string),
		Handler: DropletSizesLs,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"droplets", "ls", ":dropletId"},
		Params:  make(map[string]string),
		Handler: DoDropletsLsDroplet,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"droplets", "new", ":name", ":size", ":image", ":region", ":ssh_key_ids", ":private_networking", ":backups_enabled"},
		Params:  make(map[string]string),
		Handler: DoDropletsNewDroplet,
	})

	RoutingTable = append(RoutingTable, &Route{
    Pattern: []string{"droplets", "destroy", ":droplet_id", ":scrub_data"},
		Params:  make(map[string]string),
		Handler: DoDropletsDestroyDroplet,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"droplets", "ls"},
		Params:  make(map[string]string),
		Handler: DoDropletsLs,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"images", "ls"},
		Params:  make(map[string]string),
		Handler: DoImagesLs, })

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"images", "show", ":imageId"},
		Params:  make(map[string]string),
		Handler: DoImageShow,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"events", "show", ":eventId"},
		Params:  make(map[string]string),
		Handler: DoEventShow,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"events", "wait", ":eventId"},
		Params:  make(map[string]string),
		Handler: DoEventWait,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"regions", "ls"},
		Params:  make(map[string]string),
		Handler: DoRegionsLs,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"ssh-keys", "ls"},
		Params:  make(map[string]string),
		Handler: DoSshKeysLs,
	})

	RoutingTable = append(RoutingTable, &Route{
		Pattern: []string{"help"},
		Params:  make(map[string]string),
		Handler: ShowGeneralHelp,
	})

}

func ShowGeneralHelp(route *Route) {
	fmt.Printf("diocean <command> [arg1 [arg2 ..]] \n")
	fmt.Printf("  Commands:\n")
	for _, route := range RoutingTable {
		fmt.Printf("    %s\n", strings.Join(route.Pattern, "\t"))
		if route.HelpText != nil {
			fmt.Printf("\n")
			fmt.Print(route.HelpText)
			fmt.Printf("\n")
		}
	}
}

func ApiGet(path string, params *url.Values) (*http.Response, []byte, error) {
	if params == nil {
		params = &url.Values{}
	}
	params.Add("client_id", Config["ClientId"])
	params.Add("api_key", Config["ApiKey"])
	url := fmt.Sprintf("https://api.digitalocean.com%s?", path) + params.Encode()
	if CmdlineOptions.Verbose {
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
		fmt.Fprintf(os.Stderr, "Error reading body: %s\n", err)
		return resp, nil, err
	}

	return resp, body, nil
}

func ApiGetParsed(path string) (*http.Response, map[string]interface{}, error) {
	resp, body, err := ApiGet(path, nil)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		return resp, nil, err
	}

	var content map[string]interface{}
	json.Unmarshal(body, &content)

	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "  resp: %s\n", resp)
	}
	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "  content: %s\n", content)
	}

	if content["status"] != "OK" {
		return resp, content, errors.New(fmt.Sprintf("Request Failed (not OK): status=%s", content["status"]))
	}

	return resp, content, nil
}

func MapGetString(m map[string]interface{}, k string, defaultValue string) string {
	val, ok := m[k]
	if ok && val != nil {
		return val.(string)
	}

	return defaultValue
}

func DoImagesLs(route *Route) {
  path := "/images/"
	_, body, err := ApiGet(path, nil)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
	}

  var resp ImagesResponse
  resp.Unmarshal(body)

	fmt.Print(strings.Join(resp.Header(), "\t"))
	fmt.Print("\n")

	for _, image := range resp.Images {
    fmt.Print(strings.Join(image.ToStringArray(), "\t"))
		fmt.Print("\n")
	}
}

func DoImageShow(route *Route) {
  path := fmt.Sprintf("/images/%s", route.Params["imageId"])
	_, body, err := ApiGet(path, nil)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
	}

  var resp ImageShowResponse
  resp.Unmarshal(body)
	fmt.Print(strings.Join(resp.Header(), "\t"))
	fmt.Print("\n")
	fmt.Print(strings.Join(resp.Image.ToStringArray(), "\t"))
	fmt.Print("\n")
}

func EventShow(route *Route) *EventResponse {
  path := fmt.Sprintf("/events/%s/", route.Params["eventId"])
	_, body, err := ApiGet(path, nil)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
	}

	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "body=%s\n", body)
	}

	var resp EventResponse
	resp.Unmarshal(body)
	if resp.Status != "OK" {
		fmt.Fprintf(os.Stderr, "Error: status != OK status=%s resp=%s\n", resp.Status, string(body))
		os.Exit(1)
	}

	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "resp=%s\n", resp)
	}

  return &resp
}

func DoEventShow(route *Route) {
  resp := EventShow(route)

	fmt.Print(strings.Join(resp.Header(), "\t"))
	fmt.Print("\n")
  fmt.Print(strings.Join(resp.Event.ToStringArray(), "\t"))
	fmt.Print("\n")
}

func DoEventWait(route *Route) {
  resp := EventShow(route)

	fmt.Print(strings.Join(resp.Header(), "\t"))
	fmt.Print("\n")
  for ;; {
    fmt.Print(strings.Join(resp.Event.ToStringArray(), "\t"))
    fmt.Print("\n")
    if (resp.Event.Percentage == "100") {
      break
    }
    resp = EventShow(route)
  }
}

func DoDropletsLs(route *Route) {
  path := "/droplets/"
	_, body, err := ApiGet(path, nil)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
	}

	var resp ActiveDropletsResponse
	resp.Unmarshal(body)

	if resp.Status != "OK" {
		fmt.Fprintf(os.Stderr, "Error: status != OK status=%s resp=%s\n", resp.Status, string(body))
		os.Exit(1)
	}

	fmt.Printf("%s\n", strings.Join(resp.Header(), "\t"))
	for _, droplet := range resp.Droplets {
		fmt.Print(strings.Join(droplet.ToStringArray(), "\t"))
		fmt.Print("\n")
	}
}

func DoDropletsDestroyDroplet (route *Route) {
	params := &url.Values{}
	if CmdlineOptions.Verbose {
    fmt.Printf("DoDropletsDestroyDroplet: route=%s\n", route)
  }
	params.Add("scrub_data", route.Params["scrub_data"])

  path := fmt.Sprintf("/droplets/%s/destroy/", route.Params["droplet_id"])
	_, body, err := ApiGet(path, params)

  if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
  }

  var resp DestroyDropletResponse
  resp.Unmarshal(body)

	if resp.Status != "OK" {
		fmt.Fprintf(os.Stderr, "Error: status != OK status=%s resp=%s\n", resp.Status, string(body))
		os.Exit(1)
	}

  fmt.Print(strings.Join(resp.Header(), "\t"))
  fmt.Print("\n")
  fmt.Printf("%.f", resp.Event_id)
  fmt.Print("\n")
}

func DoDropletsNewDroplet(route *Route) {
	params := &url.Values{}
	params.Add("name", route.Params["name"])

  matched, err := regexp.MatchString("^\\d+$", route.Params["size"])
  if err != nil {
		fmt.Fprintf(os.Stderr, "Error regex match failed: %s\n", err)
		os.Exit(1)
  }

  if matched {
    params.Add("size_id", route.Params["size"])
  } else {
    params.Add("size_slug", route.Params["size"])
  }

  matched, err = regexp.MatchString("^\\d+$", route.Params["image"])
  if err != nil {
		fmt.Fprintf(os.Stderr, "Error regex match failed: %s\n", err)
		os.Exit(1)
  }

  if matched {
    params.Add("image_id", route.Params["image"])
  } else {
    params.Add("image_slug", route.Params["image"])
  }

  matched, err = regexp.MatchString("^\\d+$", route.Params["region"])
  if err != nil {
		fmt.Fprintf(os.Stderr, "Error regex match failed: %s\n", err)
		os.Exit(1)
  }

  if matched {
    params.Add("region_id", route.Params["region"])
  } else {
    params.Add("region_slug", route.Params["region"])
  }

  params.Add("ssh_key_ids", route.Params["ssh_key_ids"])
  params.Add("private_networking", route.Params["private_networking"])
  params.Add("backups_enabled", route.Params["backups_enabled"])

	_, body, err := ApiGet("/droplets/new", params)

  var resp NewDropletResponse
  resp.Unmarshal(body)

	if resp.Status != "OK" {
		fmt.Fprintf(os.Stderr, "Error: status != OK status=%s resp=%s\n", resp.Status, string(body))
		os.Exit(1)
	}

  fmt.Print(strings.Join(resp.Header(), "\t"))
  fmt.Print("\n")
  fmt.Print(strings.Join(resp.Droplet.ToStringArray(), "\t"))
  fmt.Print("\n")

}

func DoDropletsLsDroplet(route *Route) {
	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "DoDropletsLsDroplet %s\n", route)
	}
  path := fmt.Sprintf("/droplets/%s", route.Params["dropletId"])
	_, body, err := ApiGet(path, nil)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
	}

  var resp DropletShowResponse
  resp.Unmarshal(body)

	fmt.Print(strings.Join(resp.Header(), "\t"))
	fmt.Print("\n")
	fmt.Print(strings.Join(resp.Droplet.ToStringArray(), "\t"))
	fmt.Print("\n")
}

func DropletSizesLs(route *Route) {
	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "SizesLs: %s\n", route)
	}

  path := "/sizes/"
	_, body, err := ApiGet(path, nil)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
	}

	var resp DropletSizesResponse
	resp.Unmarshal(body)
	if resp.Status != "OK" {
		fmt.Fprintf(os.Stderr, "Error: status != OK status=%s resp=%s\n", resp.Status, string(body))
		os.Exit(1)
	}

	fmt.Print(strings.Join(resp.Header(), "\t"))
	fmt.Print("\n")
	for _, size := range resp.Sizes {
		fmt.Print(strings.Join(size.ToStringArray(), "\t"))
		fmt.Print("\n")
	}
}

func DoRegionsLs(route *Route) {

  path := "/regions/"
	_, body, err := ApiGet(path, nil)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
	}

	var resp RegionResponse
	resp.Unmarshal(body)
	if resp.Status != "OK" {
		fmt.Fprintf(os.Stderr, "Error: status != OK status=%s resp=%s\n", resp.Status, string(body))
		os.Exit(1)
	}

	fmt.Print(strings.Join(resp.Header(), "\t"))
	fmt.Print("\n")
	for _, region := range resp.Regions {
		fmt.Print(strings.Join(region.ToStringArray(), "\t"))
		fmt.Print("\n")
	}
}

func DoSshKeysLs(route *Route) {
  path := "/ssh_keys/"
	_, body, err := ApiGet(path, nil)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error performing http.get[%s]: %s\n", path, err)
		os.Exit(1)
	}

	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "body=%s\n", body)
	}

	var resp SshKeysResponse
	resp.Unmarshal(body)
	if resp.Status != "OK" {
		fmt.Fprintf(os.Stderr, "Error: status != OK status=%s resp=%s\n", resp.Status, string(body))
		os.Exit(1)
	}

	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "resp=%s\n", resp)
	}

	fmt.Print(strings.Join(resp.Header(), "\t"))
	fmt.Print("\n")
	for _, sshKey := range *resp.Ssh_keys {
		fmt.Print(strings.Join(sshKey.ToStringArray(), "\t"))
		fmt.Print("\n")
	}
}

func RouteMatches(route *Route, args []string) (*Route, bool) {
	if CmdlineOptions.Verbose {
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
		if CmdlineOptions.Verbose {
			fmt.Fprintf(os.Stderr, "  part:%s arg:%s rest:%s\n", part, arg, res.Args)
		}
		if strings.HasPrefix(part, ":") {
			res.Params[part[1:]] = arg
			continue
		}

		if part == arg {
			continue
		}

		if CmdlineOptions.Verbose {
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
	flag.BoolVar(&CmdlineOptions.Verbose, "v", false, "Verbose")
	InitRoutingTable()
	flag.Parse()
	route := FindMatchingRoute(flag.Args())
	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "Args: %s\n", flag.Args())
	}
	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "Route: %s\n", route)
	}

	if route == nil {
		fmt.Fprintf(os.Stderr, "Error: unrecognized command: %s\n", flag.Args())
		ShowGeneralHelp(route)
		os.Exit(1)
	}

	if !InitConfig() {
		fmt.Fprintf(os.Stderr, "Invalid or Missing configuration file.\n")
		os.Exit(1)
	}
	if CmdlineOptions.Verbose {
		fmt.Fprintf(os.Stderr, "Config: %s\n", Config)
	}

	if route != nil {
		if CmdlineOptions.Verbose {
			fmt.Fprintf(os.Stderr, "Calling route: %s\n", route)
		}
		route.Handler(route)
	}
}
