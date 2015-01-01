package main

import (
	"encoding/json"
	"fmt"
	"github.com/kyleburton/argv-router"
	"io/ioutil"
	"os"
	"sort"
	//"strings"
)

type ConfigType map[string]string

var Config ConfigType

func LoadConfiguration() {
	if !FileExists(CommandLineOptions.ConfigFile) {
		msg := fmt.Sprintf("Error: configuration file (%s) does not exist!", CommandLineOptions.ConfigFile)
		fmt.Fprintf(os.Stderr, "%s\n", msg)
		fmt.Fprintf(os.Stderr, "Please create the file with the following properties:\n\n")
		fmt.Fprintf(os.Stderr, SampleConfigurationFile)
		fmt.Fprintf(os.Stderr, "\n\n")
		panic(msg)
	}

	file, e := ioutil.ReadFile(CommandLineOptions.ConfigFile)

	if e != nil {
		fmt.Fprintf(os.Stderr, "File error: %v\n", e)
		os.Exit(1)
	}

	json.Unmarshal(file, &Config)

	// TODO: validate the config has host (non-empty string, resolves via dns?)
	// TODO: validate the config has user (non-empty string)
	// TODO: validate the config has pass (non-empty string)
	_, ok := Config["scheme"]
	if !ok {
		Config["scheme"] = "https"
	}

	_, ok = Config["port"]
	if !ok {
		Config["port"] = "443"
	}

	_, ok = Config["version"]
	if !ok {
		Config["version"] = "2"
	}
}

func CmdShowConfig(route *argvrouter.Route) {
	fmt.Fprintf(os.Stderr, "CmdShowConfig\n")

	var keys []string
	for k := range Config {
		keys = append(keys, k)
	}

	sort.Strings(keys)
	for _, k := range keys {
		fmt.Fprintf(os.Stderr, "  %s=%s\n", k, Config[k])
	}
}
