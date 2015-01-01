package main

import (
	"flag"
	"fmt"
	"github.com/kyleburton/argv-router"
	"os"
	"strings"
)

type CommandLineOptionsStruct struct {
	ConfigFile string
}

var CommandLineOptions CommandLineOptionsStruct = CommandLineOptionsStruct{}
var SampleConfigurationFile string = `
{
  "jira": {
    "host": 	      "your-jira-instance.com",
    "port": 	      null,
    "api_root":    "rest",
    "api_name":    "api",
    "api_version": "2",
    "user":        "your-name",
    "pass":        "y0urp4$$w0rD"
  }
}
`

func FileExists(fname string) bool {
	_, err := os.Stat(fname)
	if os.IsNotExist(err) {
		return false
	}
	return true
}

func LoadConfiguration() {
	if !FileExists(CommandLineOptions.ConfigFile) {
		msg := fmt.Sprintf("Error: configuration file (%s) does not exist!", CommandLineOptions.ConfigFile)
		fmt.Fprintf(os.Stderr, "%s\n", msg)
		fmt.Fprintf(os.Stderr, "Please create the file with the following properties:\n\n")
		fmt.Fprintf(os.Stderr, SampleConfigurationFile)
		fmt.Fprintf(os.Stderr, "\n\n")
		panic(msg)
	}
	fmt.Printf("Loading Conifguration: %s\n", CommandLineOptions.ConfigFile)
}

func CmdListIssue(route *argvrouter.Route) {
	var issueId = route.Params["id"]
	fmt.Fprintf(os.Stderr, "ListIssueCmd: id=%s\n", issueId)
}

func CmdShowConfig(route *argvrouter.Route) {
	fmt.Fprintf(os.Stderr, "CmdShowConfig\n")
}

func BuildRoutingTable() {
	argvrouter.AddRoute(&argvrouter.Route{
		Pattern: []string{"ls", "issue", ":id"},
		Handler: CmdListIssue,
	})

	argvrouter.AddRoute(&argvrouter.Route{
		Pattern: []string{"show", "config"},
		Handler: CmdShowConfig,
	})
}

func init() {
	BuildRoutingTable()
}

func CmdShowHelp(route *argvrouter.Route) {
	fmt.Fprintf(os.Stdout, "\n")

	for _, route := range argvrouter.RoutingTable {
		fmt.Fprintf(os.Stdout, "\t%s\n", strings.Join(route.Pattern, " "))
	}
	fmt.Fprintf(os.Stdout, "\n")
}

func main() {
	defaultConfigPath := os.Getenv("HOME") + "/.krb.jira.json"
	flag.StringVar(&CommandLineOptions.ConfigFile, "conf", defaultConfigPath, "Specify an alternate configuration file.")

	flag.Parse()

	LoadConfiguration()

	if len(flag.Args()) == 0 {
		CmdShowHelp(nil)
		os.Exit(1)
	}

	route := argvrouter.FindMatchingRoute(flag.Args())

	if nil == route {
		fmt.Fprintf(os.Stderr, "Error: unrecognized command: %s\n", strings.Join(flag.Args(), " "))
		os.Exit(1)
	}

	route.Handler(route)
	os.Exit(0)
}
