package stevenson

import(
  "time"
  "fmt"
  "strings"
  "io/ioutil"
	"launchpad.net/goyaml"
)

type FileInfo struct {
  SrcPath string
  DstPath string
  LastModified time.Time
  IsTemplate bool
  HasFrontMatter bool
  ContentStart int
  Dependencies []FileInfo
  DependedOnBy []FileInfo
  Attributes   map[interface{}]interface{}
  Builder      func(FileInfo) error
}

var ProjectFiles map[string]FileInfo

func init() {
  ProjectFiles = make(map[string]FileInfo)
}

func (self FileInfo) DestinationPath() string {
  p := self.SrcPath[len(Configuration.SourcePath):]
  if strings.HasPrefix(p,"/") {
    return Configuration.DestinationPath + p
  }

  return Configuration.DestinationPath + "/" + p
}

func (self FileInfo) String() string {
  var isTemplate = "false"
  var hasFrontmatter = "false"
  if self.IsTemplate {
    isTemplate = "true"
  }
  if self.HasFrontMatter {
    hasFrontmatter = "true"
  }
  return fmt.Sprintf("FileInfo{SrcPath=%s; DstPath=%s, LastModified=%q, IsTemplate=%s, HasFrontMatter=%s, Dependencies=%q, DependedOnBy=%q, Attributes=%q}",
  self.SrcPath,
  self.DstPath,
  self.LastModified,
  isTemplate,
  hasFrontmatter,
  self.Dependencies,
  self.DependedOnBy,
  self.Attributes)
}

func NewFromFile (path string) (fi FileInfo) {
  fi = FileInfo{
        SrcPath:          path,
        DstPath:          "...compute dest path...",
        IsTemplate:       false,
        HasFrontMatter:   false,
        Dependencies:     []FileInfo{},
        DependedOnBy:     []FileInfo{},
  }

  fi.DstPath = fi.DestinationPath()

  data, err := ioutil.ReadFile(path)
  if err != nil {
    panic(fmt.Sprintf("Error reading file %s : %v", path, err))
  }

  content := string(data)
  fi.HasFrontMatter = strings.HasPrefix(content,"---\n")

  if fi.HasFrontMatter {
    spos := strings.Index(content, "---\n") + len("---\n")
    epos := spos + strings.Index(content[spos:], "---\n")
    frontMatter := content[spos:epos]
    fi.ContentStart = epos + len("---\n")
    goyaml.Unmarshal([]byte(frontMatter), &fi.Attributes)
  }

  for _, ext := range(Configuration.ProcessFileExtensions) {
    if strings.HasSuffix(path, ext) {
      if strings.Contains(content, "{%") {
        fi.IsTemplate = true
      }
      if strings.Contains(content, "{{") {
        fi.IsTemplate = true
      }
      break
    }
  }

  switch {
  case fi.IsTemplate && fi.HasFrontMatter:
    fi.Builder = ProcessLiquidFileWithFrontMatter
  case fi.IsTemplate:
    fi.Builder = ProcessFileWithFrontMatter
  case fi.HasFrontMatter:
    fi.Builder = ProcessLiquidFile
  default:
    fi.Builder = ProcessRawFile
  }

  if fi.IsTemplate && fi.HasFrontMatter {
  }

  return
}


func (self FileInfo) GetContent() (string, error) {
  data, err := ioutil.ReadFile(self.SrcPath)
  if err != nil  {
    return "", err
  }

  if self.HasFrontMatter {
    return string(data)[self.ContentStart:], nil
  }

  return string(data), nil
}
