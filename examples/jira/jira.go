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

func CmdShowHelp(route *argvrouter.Route) {
	fmt.Fprintf(os.Stdout, "\n")

	for _, route := range argvrouter.RoutingTable {
		fmt.Fprintf(os.Stdout, "\t%s\n", strings.Join(route.Pattern, " "))
	}

	fmt.Fprintf(os.Stdout, "\n")
}

func BuildRoutingTable() {
	argvrouter.AddRoute(&argvrouter.Route{Pattern: []string{"issue", "ls", ":id"}, Handler: CmdListIssue})
	argvrouter.AddRoute(&argvrouter.Route{Pattern: []string{"i", "ls", ":id"}, Handler: CmdListIssue})

	argvrouter.AddRoute(&argvrouter.Route{Pattern: []string{"issues", "search", "*"}, Handler: CmdSearchIssues})
	argvrouter.AddRoute(&argvrouter.Route{Pattern: []string{"i", "search", "*"}, Handler: CmdSearchIssues})
	argvrouter.AddRoute(&argvrouter.Route{Pattern: []string{"i", "s", "*"}, Handler: CmdSearchIssues})

	argvrouter.AddRoute(&argvrouter.Route{Pattern: []string{"show", "config"}, Handler: CmdShowConfig})
}

func init() {
}

func main() {
	defaultConfigPath := os.Getenv("HOME") + "/.krb.jira.json"
	flag.StringVar(&CommandLineOptions.ConfigFile, "conf", defaultConfigPath, "Specify an alternate configuration file.")

	flag.Parse()

	if len(flag.Args()) == 0 {
		CmdShowHelp(nil)
		os.Exit(1)
	}

	BuildRoutingTable()

	route := argvrouter.FindMatchingRoute(flag.Args())

	if nil == route {
		fmt.Fprintf(os.Stderr, "Error: unrecognized command: %s\n", strings.Join(flag.Args(), " "))
		os.Exit(1)
	}

	LoadConfiguration()
	InitApi()
	route.Handler(route)
	os.Exit(0)
}
