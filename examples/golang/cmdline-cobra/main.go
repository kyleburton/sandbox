// go get github.com/spf13/cobra
package main

import (
	"fmt"
	"github.com/spf13/cobra"
)

func RunCmdLs(cmd *cobra.Command, args []string) {
	fmt.Printf("RunCmdLs: args=%s\n", args)
}

func main() {
	fmt.Printf("OK\n")
	var cmdLs = &cobra.Command{
		Use:   "ls [type]",
		Short: "List items from the remote system",
		Long: `ls is for listing items from the remote api.
by default it will create tab delimited output suitable for use with the
standard unix toolset.
		`,
		Run: RunCmdLs,
	}

	// TODO: explore Flags
	// cmdLs.Flags().StringVarP()

	rootCmd := &cobra.Command{Use: "jira"}
	rootCmd.AddCommand(cmdLs)
	rootCmd.Execute()
}
