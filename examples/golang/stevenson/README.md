# Stevenson

Static site generator written in golang.  The goal is to optimize for interactive develolpment and medium to large sized sites.

# Installation

You must have go installed and on your `$PATH`.

# Configuration

`stevenson` uses a YAML configuration file (`.stevenson.yaml` in the current working directory by default) and supports the following options:

### `destinationDirectory` = path

Specify the output directory.

### `processFilesWithExtension` = [ ... ]

Default: `[".html"]`

Any file names matching these extensions will be processed as templates.  All other file types will be moved to the destination directory as-is.

# Approach

`stevenson` watches the site directory and tracks the following information on each file:

### SrcPath string

The relative path of the file

### DstPath string

The destination path.

### LastModified time.Time

The time of last modification.

### IsTemplate bool

True if the file should be processed as a template.

### Dependencies []FileInfo

The list of files that this file depends on.  If any of the dependencies change, this file will need to be re-generated.

### DependedOnBy []FileInfo

The list of files that are depended on by this file.  If this file changes, each of the dependencies will need to be re-generated.

## Maintenence of the Target Directory

Unprocessed files are 'published' to the destination directory by hard-linking.  This reduces on-disk storage and is significantly faster than copying the files themselves.

