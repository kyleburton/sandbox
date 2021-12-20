package main

import (
	"fmt"
	"github.com/spf13/viper"
)

type JiraApiInfo struct {
	Version string
	Prefix  string
}

type ConfigInfo struct {
	Host    string      `host`
	Port    int         `port`
	User    string      `user`
	Pass    string      `pass`
	ApiInfo JiraApiInfo `apiInfo`
}

func main() {
	viper.SetDefault("ConfigFile", "$HOME/.krb.jira.json")
	viper.SetConfigName(".krb.jira")
	viper.SetConfigType("json")
	//viper.AddConfigPath("$HOME")
	viper.AddConfigPath(".")
	viper.ReadInConfig()

	fmt.Printf("host:%s\n", viper.GetString("host"))
	fmt.Printf("port:%d\n", viper.GetInt("port"))
	fmt.Printf("user:%s\n", viper.GetString("user"))
	fmt.Printf("pass:%s\n", viper.GetString("pass"))

	apiInfo := &JiraApiInfo{}
	viper.MarshalKey("apiInfo", &apiInfo)
	fmt.Printf("apiInfo:%q\n", apiInfo)

	// NB: marshaling this whole struct doesn't seem to work?
	/*
		config := &ConfigInfo{}
		err := viper.Marshal(&config)
		if err != nil {
			panic(err)
		}
		fmt.Printf("config:%q\n", config)
	*/
}
