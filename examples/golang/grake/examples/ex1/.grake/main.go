package main

import (
	"flag"
	"fmt"
	f "grake/fileutils"
	g "grake/tasks"
	"io/ioutil"
	"strings"
)

var _ = f.Exists // ignore if unused
var _ = g.Desc   // ignore if unused
var _ = strings.Join

func WriteFile(f, s string) {
	ioutil.WriteFile(f, []byte(s), 0644)
}

func make_all_tasks() {
	// vim: ft=go

	g.Desc("not in a namespace")
	g.Task("hello", func(self *g.TaskInfo) {
		fmt.Printf("in task: %s\n", self.Name)
	})

	g.Namespace("main", func() {
		g.Desc("This is a test task")
		g.Task("hello", func(self *g.TaskInfo) {
			fmt.Printf("Hello! from: %s\n", self.Name)
		})
		g.Depends("hello")

		g.Namespace("two", func() {
			g.Desc("a task with arguments")
			g.Task("hasargs[a,b,c]", func(self *g.TaskInfo) {
				fmt.Printf("%s, self.Args: %q\n", self.Name, self.Args)
				fmt.Printf("%s, and I have arguments: a=%s, b=%s, c=%s\n", self.Name, self.Args["a"], self.Args["b"], self.Args["c"])
			})
			g.Depends("main:hello")
		})
	})

	g.Desc("make a file")
	g.Task("createfile", func(self *g.TaskInfo) {
		txt :=
			`this is 
the 
file contents`
		WriteFile("output.txt", txt)
	})

	g.Default("main:hello")
}

func main() {
	showTasks := flag.Bool("T", false, "Show Tasks")
	showHelp := flag.Bool("help", false, "Show Help")
	flag.Parse()

	if *showHelp {
		fmt.Printf("Show Help here\n")
		return
	}

	make_all_tasks()

	taskName := g.DefaultTaskName
	if len(flag.Args()) > 0 {
		taskName = flag.Args()[0]
	}

	taskName, args := g.ParseTaskString(taskName)

	if *showTasks {
		g.ShowTasks()
		return
	}

	g.InvokeTask(taskName, args)
}
