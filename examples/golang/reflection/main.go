// http://merbist.com/2011/06/27/golang-reflection-exampl/
// https://golang.org/pkg/reflect/
package main

import (
	"encoding/json"
	"fmt"
)

type DataSource struct {
	Name     string
	DsId     string
	Host     string
	Driver   string
	Port     int32
	Username string
	Password string
}

type APIKey struct {
	KeyProvider string
	KeyType     string
	Key         string
	KeySecret   string
}

type FeatureSwitch struct {
	Name    string
	Enabled bool
}

type Configuration struct {
	Environment string
	DataSources []DataSource
	APIKeys     []APIKey
	Features    []FeatureSwitch
}

func PrettyPrintInterface(data interface{}) string {
	res, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		panic(err)
	}

	return string(res)
}

func main() {
	cfg := &Configuration{
		Environment: "development",
		DataSources: []DataSource{
			DataSource{
				Name:     "UsersDs",
				DsId:     "users_dev",
				Host:     "localhost",
				Driver:   "pgsql",
				Port:     5432,
				Username: "dev-user-uname",
				Password: "dev-user-pass",
			},
			DataSource{
				Name:     "EventsDs",
				DsId:     "events_dev",
				Host:     "localhost",
				Driver:   "pgsql",
				Port:     5432,
				Username: "evt-user-uname",
				Password: "evt-user-pass",
			},
		},
		APIKeys: []APIKey{
			APIKey{
				KeyProvider: "aws",
				KeyType:     "aws-access-key",
				Key:         "NYJPZNAZUQCQGTDBLSPLM",
				KeySecret:   "4LZTi5M8/DS6mRYNL7kHC5rM9B+4aieWAM3uVDM9",
			}},
		Features: []FeatureSwitch{},
	}

	fmt.Printf("cfg: %s\n", PrettyPrintInterface(cfg))
}
