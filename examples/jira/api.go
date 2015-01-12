package main

import (
	"encoding/json"
	"fmt"
	"github.com/fatih/color"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"golang.org/x/crypto/ssh/terminal"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strings"
)

var Client *http.Client

type JiraResponse map[string]interface{}

var Blue = color.New(color.FgBlue).SprintfFunc()
var Cyan = color.New(color.FgCyan).SprintfFunc()

func init() {
	Client = &http.Client{}

	if !terminal.IsTerminal(int(os.Stdout.Fd())) {
		Blue = fmt.Sprintf
		Cyan = fmt.Sprintf
	}
}

func MakeUrl(path []string, params *url.Values) string {
	if nil == params {
		params = &url.Values{}
	}
	parts := append([]string{
		fmt.Sprintf("%s://%s:%s/rest/api/%s", viper.GetString("scheme"), viper.GetString("host"), viper.GetString("port"), Version)}, path...)
	return strings.Join(parts, "/") + "?" + params.Encode()
}

func ApiGet(path []string, params *url.Values) JiraResponse {
	url := MakeUrl(path, params)
	//fmt.Fprintf(os.Stderr, "ApiGet url=%s\n", url)

	req, err := http.NewRequest("GET", url, nil)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error[GET:%s]: %s\n", url, err)
		os.Exit(1)
	}

	req.SetBasicAuth(viper.GetString("user"), viper.GetString("pass"))

	resp, err := Client.Do(req)

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading body: %s\n", err)
		os.Exit(1)
	}

	//fmt.Fprintf(os.Stderr, "Response: %v\n", resp)

	var results map[string]interface{}

	json.Unmarshal(body, &results)
	return results
}

type StringToInterfaceMap map[string]interface{}

func GetKeys(self map[string]interface{}) []string {
	var keys []string
	keys = make([]string, 0)
	for k, _ := range self {
		keys = append(keys, k)
	}
	return keys
}

func GetPath(m map[string]interface{}, path ...string) string {
	for ii := 0; ii < len(path)-1; ii++ {
		p := path[ii]
		m = m[p].(map[string]interface{})
	}
	return m[path[len(path)-1]].(string)
}

func (self JiraResponse) PrintIssueSearchResults() {
	if Verbose {
		body, err := json.MarshalIndent(self, "", "  ")

		if err != nil {
			panic(err)
		}

		fmt.Fprintf(os.Stdout, string(body))
	}

	issues := self["issues"].([]interface{})
	//fmt.Fprintf(os.Stderr, "Got Back %d issues\n", len(issues))
	fmt.Fprintf(os.Stdout, "%s\n", strings.Join([]string{"id", "name", "summary", "status"}, "\t"))
	for _, issue := range issues {
		item := issue.(map[string]interface{})
		//fmt.Fprintf(os.Stdout, "  item.keys=%s\n", strings.Join(GetKeys(item), ","))
		fields := item["fields"].(map[string]interface{})
		//fmt.Fprintf(os.Stdout, "  fields.keys=%s\n", strings.Join(GetKeys(fields), ","))
		assignee := fields["assignee"].(map[string]interface{})
		// status -> name
		fmt.Fprintf(os.Stdout, "%s\n", strings.Join(
			[]string{
				Cyan(item["key"].(string)),
				assignee["name"].(string),
				fields["summary"].(string),
				GetPath(fields, "status", "name"),
			}, "\t"))
	}
}

// https://docs.atlassian.com/jira/REST/6.3.12/#d2e3662
// https://confluence.atlassian.com/display/JIRA/Advanced+Searching
func JiraSearch(cmd *cobra.Command, args []string) {
	//fmt.Fprintf(os.Stderr, "SearchIssues: terms=%v\n", args)
	params := &url.Values{}
	params.Add("jql", strings.Join(args, " "))
	params.Add("startAt", "0")
	params.Add("maxResults", "25")
	params.Add("validateQuery", "true")
	//params.Add("fields", "")
	//params.Add("expand", "")

	results := ApiGet([]string{"search"}, params)

	results.PrintIssueSearchResults()
}
