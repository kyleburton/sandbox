package main

/*
  For configuration:          github.com/spf13/viper
	For the cmdline arg/router: github.com/spf13/cobra

*/
import (
	"fmt"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"os"
)

var JiraCmd = &cobra.Command{
	Use:   "jira",
	Short: "Jira is a command line tool for interacting with Jira",
	Long: `Jira implements a command line interface to Jira for searching, 
creating, and modifying cards.
	`,
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Printf("...show help here...\n")
	},
}

var Verbose bool = false
var Version string = "2"

func init() {
	viper.SetConfigName(".krb.jira")
	viper.SetDefault("scheme", "https")
	viper.SetDefault("port", "443")
	viper.SetDefault("user", os.Getenv("LOGNAME"))
	viper.SetDefault("url_root", "/rest/api/2")
	viper.SetDefault("version", "2")
	viper.AddConfigPath(".")
	viper.AddConfigPath("$HOME")

	var listCmd = &cobra.Command{
		Use:   "list",
		Short: "List issues",
		Long:  `List issues`,
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println("List Issues using the api...")
		},
	}

	JiraCmd.PersistentFlags().BoolVarP(&Verbose, "verbose", "v", Verbose, "Be verbose")
	JiraCmd.PersistentFlags().StringVar(&Version, "version", Version, "Set the api version")

	JiraCmd.AddCommand(listCmd)

	var searchCmd = &cobra.Command{
		Use:   "search",
		Short: "Search Issues",
		Long:  `Search issues using the Jira Query Language`,
		Run:   JiraSearch,
	}

	JiraCmd.AddCommand(searchCmd)

	var viewCmd = &cobra.Command{
		Use:   "view",
		Short: "View an issue's details",
		Long:  `View an issue's details`,
		Run:   JiraViewItem,
	}

	JiraCmd.AddCommand(viewCmd)
}

func ShowConfig() {
	fmt.Printf("Configuration\n")
	fmt.Printf("  verbose=%v\n", viper.GetBool("verbose"))
	fmt.Printf("  scheme=%s\n", viper.GetString("scheme"))
	fmt.Printf("  host=%s\n", viper.GetString("host"))
	fmt.Printf("  user=%s\n", viper.GetString("user"))
	fmt.Printf("  pass=%s\n", viper.GetString("pass"))
	fmt.Printf("  port=%s\n", viper.GetString("port"))
}

func main() {
	viper.ReadInConfig()

	/*
		var verbose bool = false
		var showConfig bool = false
		var host string = viper.GetString("host")
		var user string = viper.GetString("user")
	*/

	//flag.BoolVar(&showConfig, "show-config", showConfig, "Show configuration")
	//flag.BoolVar(&verbose, "verbose", verbose, "Be verbose.")
	//flag.StringVar(&host, "host", viper.GetString("host"), "Set the hostname.")
	//flag.StringVar(&user, "user", viper.GetString("user"), "Set the Jira user.")

	//flag.Parse()

	/*
		viper.Set("verbose", verbose)
		viper.Set("host", host)
		viper.Set("user", user)
	*/

	//	if showConfig {
	//		ShowConfig()
	//		os.Exit(0)
	//	}

	JiraCmd.Execute()
}
