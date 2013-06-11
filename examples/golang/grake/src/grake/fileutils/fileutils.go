package fileutils

import (
    "os"
)

func Exists ( filename string ) bool {
  if _, err := os.Stat(filename); os.IsNotExist(err) {
    return false
  }
  return true
}
