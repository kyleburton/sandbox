package stevenson

import(
  "fmt"
  "os"
	"launchpad.net/goyaml"
	"io/ioutil"
)

type StevensonConfiguration struct {
  ConfigPath string
  SourcePath string
  DestinationPath string
  ProcessFileExtensions []string
  AutoMode bool
}

var Configuration StevensonConfiguration

func (self StevensonConfiguration) String() string {
  return fmt.Sprintf("StevensonConfiguration{SourcePath=%s, DestinationPath=%s, ProcessFileExtensions=%q}",
  self.SourcePath,
  self.DestinationPath,
  self.ProcessFileExtensions)
}

func Exists ( filename string ) bool {
  if _, err := os.Stat(filename); os.IsNotExist(err) {
    return false
  }
  return true
}


func LoadConfiguration(configPath *string) {
  if !Exists(*configPath) {
    return
  }

  m := make(map[interface{}]interface{})
  data, err := ioutil.ReadFile(*configPath)
  if err != nil {
    panic(fmt.Sprintf("Error reading file %s : %v", configPath, err))
  }

  goyaml.Unmarshal([]byte(data), &m)

  if val, ok := m["sourceDirectory"]; ok {
    Configuration.SourcePath = val.(string)
  }

  if val, ok := m["destinationDirectory"]; ok {
    Configuration.DestinationPath = val.(string)
  }

  if val, ok := m["processFilesWithExtension"]; ok {
    vals := val.([]interface{})
    exts := make([]string, len(vals))
    for idx, val := range(vals) {
      exts[idx] = val.(string)
    }
    Configuration.ProcessFileExtensions = exts
  }
}

func init () {
  Configuration = StevensonConfiguration{
    ConfigPath:            "",
    SourcePath:            ".",
    DestinationPath:       "_site",
    ProcessFileExtensions: []string{".html"},
  }
}

