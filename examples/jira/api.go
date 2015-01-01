package main

import (
	"encoding/json"
	"fmt"
	"github.com/kyleburton/argv-router"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strings"
)

var Client *http.Client

func InitApi() {
	Client = &http.Client{}
}

func (self ConfigType) MakeUrl(path []string, params *url.Values) string {
	if nil == params {
		params = &url.Values{}
	}
	parts := append([]string{
		fmt.Sprintf("%s://%s:%s/rest/api/%s", Config["scheme"], Config["host"], Config["port"], Config["version"])}, path...)
	return strings.Join(parts, "/") + "?" + params.Encode()
}

func (self ConfigType) ApiGet(path []string, params *url.Values) map[string]interface{} {
	url := Config.MakeUrl(path, params)
	fmt.Fprintf(os.Stderr, "ApiGet url=%s\n", url)

	req, err := http.NewRequest("GET", url, nil)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error[GET:%s]: %s\n", url, err)
		os.Exit(1)
	}

	req.SetBasicAuth(Config["user"], Config["pass"])

	resp, err := Client.Do(req)

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading body: %s\n", err)
		os.Exit(1)
	}

	fmt.Fprintf(os.Stderr, "Response: %v\n", resp)

	var results map[string]interface{}

	json.Unmarshal(body, &results)
	return results
}

// https://docs.atlassian.com/jira/REST/6.3.12/#d2e3662
func CmdSearchIssues(route *argvrouter.Route) {
	fmt.Fprintf(os.Stderr, "SearchIssues: terms=%v\n", route.Args)
	params := &url.Values{}
	params.Add("jql", strings.Join(route.Args, " "))
	params.Add("startAt", "0")
	params.Add("maxResults", "25")
	params.Add("validateQuery", "true")
	//params.Add("fields", "")
	//params.Add("expand", "")

	results := Config.ApiGet([]string{"search"}, params)

	body, err := json.MarshalIndent(results, "", "  ")

	if err != nil {
		panic(err)
	}

	fmt.Fprintf(os.Stdout, string(body))
}

func CmdListIssue(route *argvrouter.Route) {
	var issueId = route.Params["id"]
	fmt.Fprintf(os.Stderr, "ListIssueCmd: id=%s\n", issueId)
}
