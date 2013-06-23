package stevenson

import (
	"path"
	"path/filepath"
	"github.com/howeyc/fsnotify"
  "fmt"
  "io"
  "os"
  "log"
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
    } else {
      ProjectFiles[path] = NewFromFile(path)
    }

    return nil
  }

  filepath.Walk(path, wf)
  return nil
}

func ShouldBuild(fi FileInfo) bool {
  if !Exists(fi.DstPath) {
    return true
  }


  srcStat, err := os.Stat(fi.SrcPath)
  if err != nil {
    panic(err)
  }

  dstStat, err := os.Stat(fi.DstPath)
  if err != nil {
    panic(err)
  }

  return srcStat.ModTime().After(dstStat.ModTime())
}

func CopyFile(fi FileInfo) error {
  src := fi.SrcPath
  dst := fi.DstPath
  destDir := path.Dir(dst)
  fmt.Printf("Ensuring destDir=%s\n", destDir)
  if err := os.MkdirAll(destDir, 0755); err != nil {
    return err
  }

  s, err := os.Open(src)
  if err != nil {
    return err
  }

  defer s.Close()

  d, err := os.Create(dst)
  if err != nil {
    return err
  }

  if fi.HasFrontMatter {
    fmt.Printf("Skipping frontmatter\n")
    frontMatter := make([]byte, fi.ContentStart)
    _, err := io.ReadFull(s, frontMatter)
    if err != nil {
      return err
    }
  }

  if _, err := io.Copy(d, s); err != nil {
    d.Close()
    return err
  }
  return d.Close()
}

func ProcessFile(path string, fi FileInfo) {
  // if fi.SourcePath is newer than fi.DestinationPath, rebuild it
  // if fi.DestinationPath does not exist, rebuild it
  //fmt.Printf("Check[%s] vs it's destination: %q\n", path, fi)
  if ShouldBuild(fi) {
    fmt.Printf("!Rebuild %s => %s\n", fi.SrcPath, fi.DstPath)
    err := CopyFile(fi)
    if err != nil {
      fmt.Printf("Error building (copy) file: %s : %s\n", fi.SrcPath, err)
    }
  }
}

func ProcessAllFiles () {
  for path, fi := range(ProjectFiles) {
    ProcessFile(path,fi)
  }
}

func Run() {
  fmt.Printf("sipder and watch all the files and dirs in: %s\n", Configuration.SourcePath)

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
            log.Printf("ProcessNewFile(%s)\n", ev.Name)
          }
          if ev.IsDelete() {
            log.Printf("removing %v from the watcher\n", ev.Name)
            watcher.RemoveWatch(ev.Name)
            log.Printf("ProcessRemovedFile(%s)\n", ev.Name)
          }
          if ev.IsModify() {
            fmt.Printf("modified: %v, trigger processing\n", ev.Name)
            log.Printf("ProcessChangedFile(%s)\n", ev.Name)
            ProcessFile(ev.Name, ProjectFiles[ev.Name])
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
  recursiveAddPath(watcher, Configuration.SourcePath)

	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("watching %s\n", Configuration.SourcePath)

  // process all files into the target directory
  ProcessAllFiles()

  var ii = 0
	for {
    if 0 == ii % 5 {
      fmt.Printf("\nWatching[%d]:\n", ii)
      for _, file := range(ProjectFiles) {
        fmt.Printf("  %q\n", file)
      }
      fmt.Printf("\n\n")
    }
    ii = 1 + ii
		time.Sleep(1 * time.Second)
		fmt.Printf(".")
	}

	/* ... do stuff ... */
	watcher.Close()
}
