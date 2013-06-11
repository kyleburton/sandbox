package main

// TODO: support namespaces
// TODO: lots of helpers: system, system! chdir, chdir!, test file mtimes, test file exists
// TODO: how can we support additional imports?
import(
  "fmt"
  "strings"
  "os/exec"
)


type task struct {
	Name        string
	Description string

	block       func(t *task)
	deps        []string
}

type taskmanager map[string]*task

var TaskManager taskmanager

func (self *task) String() string {
	return fmt.Sprintf("Task{descr=\"%s\";name=\"%s\";block=%s;deps=%s",
  self.Description,
  self.Name,
  self.block,
  strings.Join(self.deps, ","))
}

func Desc(descr string) (t *task) {
	t = &task{Name: "", block: func (self *task) {}, Description: descr}
  return t
}

func (self *task) Task(name string, block func(*task)) *task {
  self.Name = name
  self.block = block
  TaskManager[name] = self
  return self
}

func Task(name string, block func(*task)) (t *task) {
	t = &task{
    Name:         name,
    block:        block,
    Description:  fmt.Sprintf("Undescribed: %q", name),
  }
  TaskManager[name] = t
  return t
}

func (self *task) Depends(deps ...string) (t *task) {
  self.deps = deps
  return self
}

// TODO: support task arguments ala rake: task[arg1,arg2]
func InvokeTask (name string) {
  if t, ok := TaskManager[name]; ok {
    ok := make(chan bool)

    // TODO: make sure a task can't depend on itself
    for _, dep := range t.deps {
      go func() {
        InvokeTask(dep)
        ok <- true
      }()
    }

    for i := 0; i < len(t.deps); i++ {
      <-ok
    }

    t.block(t)
  } else {
    panic(fmt.Sprintf("Error: no such task: %q", name))
  }
}

// TODO: handle command line arguments, take the task to run from the cmdline
// TODO: parse task arguments
func main () {
  // fmt.Printf("TaskManager: %q\n", TaskManager)
  InvokeTask("task2")
}


// TODO: how to have System that returns stdout, or a version that doesn't capture output?
func System(cmd string, args ...string) (res string) {
  out, err := exec.Command(cmd, args...).Output()
  if err != nil {
    panic(err)
  }
  return string(out)
}

func init () {
  TaskManager = make(taskmanager)
  Desc("Hello's Description").
  Task("hello", func (self *task) {
    fmt.Printf("Hello!\n")
  })

  Task("task2", func (self *task) {
    fmt.Printf("task2\n")
    fmt.Printf("ls=%s\n", System("ls", "-ltrh"))
    fmt.Printf("sleep=%s\n", System("sleep", "4"))
  }).
  Depends("hello")

  Task("task3", func (self *task) {
    fmt.Printf("task3\n")
    fmt.Printf("sleep=%s\n", System("sleep", "4"))
  }).

  Task("task4", func (self *task) {
    fmt.Printf("task4\n")
  }).
  Depends("task2", "task3")
}

