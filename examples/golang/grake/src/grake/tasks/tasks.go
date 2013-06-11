package grake

import (
  "fmt"
  "strings"
  "time"
)

var debug = false // true

type TaskInfo struct {
	Name        string
	Description string

	Block func(t *TaskInfo)
	Deps  []string
  Err   error
  Tstart     time.Time
  Tdepend    time.Time
  Tdepstart  time.Time
  Tend       time.Time
  DepsElapsd time.Duration
  Elapsd     time.Duration
  Completed  bool
  Running    bool
}

var lastDescription = "no-description"
var lastTask *TaskInfo

type taskmanager map[string]*TaskInfo

var TaskManager taskmanager

func (self *TaskInfo) String() string {
	if debug {
		fmt.Printf("Converting to String\n")
	}

	deps := self.Deps
	if nil == deps {
		deps = make([]string, 0)
	}

	if debug {
		fmt.Printf("Converting to String, deps=%q\n", deps)
	}
	return fmt.Sprintf("TaskInfo{descr=\"%s\";name=\"%s\";block=%s;deps=%s}",
		self.Description,
		self.Name,
		self.Block,
		strings.Join(deps, ","))
}

func Desc(descr string) {
  lastDescription = descr;
}

func Task(name string, block func(*TaskInfo)) (t *TaskInfo) {
	t = &TaskInfo{
		Name:        name,
		Block:       block,
		Description: lastDescription,
	}
  lastDescription = "none"
  lastTask = t
	TaskManager[name] = t
	return t
}

func Depends(deps ...string) {
	lastTask.Deps = deps
	if debug {
		fmt.Printf("Task[%s] deps: %q\n", lastTask.Name, lastTask.Deps)
	}
}

// TODO: support task arguments ala rake: task[arg1,arg2]
func InvokeTask(name string) {
	if t, ok := TaskManager[name]; ok {
    if t.Running {
      panic(fmt.Sprintf("Error: task depends on itself / circular dependency!: %s", name))
    }

    t.Running = true

    if t.Completed {
      if debug {
        fmt.Printf("InvokeTask[%s] task already completed\n", name)
      }
      return
    }

		ok := make(chan bool)

    t.Tdepstart = time.Now()

		// TODO: make sure a task can't depend on itself
		if nil != t.Deps {
			for idx, dep := range t.Deps {
				go func(name string, idx int, dep string) {
					if debug {
						fmt.Printf("InvokeTask[%s] execing dep: %d/%s\n", name, idx, dep)
					}
					InvokeTask(dep)
					ok <- true
				}(name, idx, dep)
			}

			if debug {
				fmt.Printf("InvokeTask[%s] waiting for deps\n", name)
			}
			for i := 0; i < len(t.Deps); i++ {
				<-ok
			}
		}

    t.Tdepend = time.Now()
    t.DepsElapsd = t.Tdepend.Sub(t.Tdepstart)

		if debug {
			fmt.Printf("InvokeTask[%s] executing this task\n", name)
		}

    t.Tstart = time.Now()
		t.Block(t)
    t.Tend = time.Now()
    t.Elapsd = t.Tend.Sub(t.Tstart)
    t.Completed = true

		if debug {
			fmt.Printf("InvokeTask[%s] completed in %q, deps took %q\n", 
        name, t.Elapsd, t.DepsElapsd)
		}
	} else {
		panic(fmt.Sprintf("Error: no such task: %q", name))
	}
}

func init() {
	TaskManager = make(taskmanager)
}
