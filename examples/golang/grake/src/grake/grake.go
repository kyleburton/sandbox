package grake

// TODO: support namespaces
// TODO: lots of helpers: system, system! chdir, chdir!, test file mtimes, test file exists
// TODO: how can we support additional imports?
import (
	"fmt"
	f "grake/fileutils"
	g "grake/tasks"
	"os/exec"
	// "strings"
)

var debug = false

// TODO: handle command line arguments, take the task to run from the cmdline
// TODO: parse task arguments
func xmain() {
	// fmt.Printf("TaskManager: %q\n", TaskManager)
	//InvokeTask("task2")
	g.InvokeTask("task4", make([]string, 0))
}

// TODO: how to have System that returns stdout, or a version that doesn't capture output?
func System(cmd string, args ...string) (res string) {
	out, err := exec.Command(cmd, args...).Output()
	if err != nil {
		panic(err)
	}
	return string(out)
}

func xinit() {
	g.Desc("Hello's Description")
	g.Task("hello", func(self *g.TaskInfo) {
		fmt.Printf("Hello!\n")
	})

	g.Task("task2", func(self *g.TaskInfo) {
		fmt.Printf("task2\n")
		fmt.Printf("ls=%s\n", System("ls", "-ltrh"))
		fmt.Printf("sleep=%s\n", System("sleep", "4"))
		if !f.Exists("output-file.txt") {
			fmt.Printf("")
		}
	})
	g.Depends("hello")

	g.Task("task3", func(self *g.TaskInfo) {
		fmt.Printf("task3\n")
		fmt.Printf("sleep=%s\n", System("sleep", "4"))
	})

	g.Task("task3.3", func(self *g.TaskInfo) {
		fmt.Printf("task3.3\n")
		fmt.Printf("sleep=%s\n", System("sleep", "4"))
	})

	g.Desc("Task to execute, with 3 deps")
	g.Task("task4", func(self *g.TaskInfo) {
		fmt.Printf("task4 self=%q\n", self)
	})
	g.Depends("task2", "task3", "task3.3")
}


