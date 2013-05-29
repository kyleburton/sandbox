package main

import (
	"path/filepath"
	"flag"
	"fmt"
	"github.com/howeyc/fsnotify"
	"log"
	"os"
	"time"
)

func isDir (path string) bool {
  f, err := os.Open(path)
  if err != nil {
     panic(err)
  }

  defer f.Close();

  fi, err := f.Stat()
  if err != nil {
     panic(err)
  }

  return fi.Mode().IsDir()
}

func recursiveAddPath(watcher *fsnotify.Watcher, path string) error {
	// recursivly find all dirs
  wf := func (path string, info os.FileInfo, err error) (err2 error) {
    // fmt.Printf("walk path:%v info:%v er:%v\n", path, info)
    if err2 != nil {
      return err2
    }

    if isDir(path) {
      fmt.Printf("add to watcher: path:%v info:%v er:%v\n", path, info)
	    err3 := watcher.Watch(path)
      if err3 != nil {
        panic(err3)
      }
    }

    return nil
  }

  filepath.Walk(path, wf)
  return nil
}

func main() {
	var watchDir string
	flag.StringVar(&watchDir, "watch", ".", "The directory to watch.")
	flag.Parse()

	fmt.Printf("watchDir: %v\n", watchDir)

	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("watcher: %s\n", watcher)

	// Process events
	go func() {
		for {
			select {
			case ev := <-watcher.Event:
				if ev != nil {
					log.Printf("event:%v\n", ev)
          // when a new dir shows up, add it to the watcher
          if ev.IsCreate() && isDir(ev.Name) {
             recursiveAddPath(watcher, ev.Name)
					   log.Printf("add %v to the watcher\n", ev.Name)
          }
          if ev.IsDelete() {
            log.Printf("removing %v from the watcher\n", ev.Name)
            watcher.RemoveWatch(ev.Name)
          }
          if ev.IsModify() {
            fmt.Printf("modified: %v, trigger processing\n", ev.Name)
          }
				}
			case err := <-watcher.Error:
				if err != nil {
					log.Printf("error: %v\n", err)
				}
			}
		}
	}()

	// err = watcher.Watch(watchDir)
  recursiveAddPath(watcher, watchDir)

	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("watching %s\n", watchDir)

	for {
		time.Sleep(1 * time.Second)
		fmt.Printf(".")
	}

	/* ... do stuff ... */
	watcher.Close()
}
