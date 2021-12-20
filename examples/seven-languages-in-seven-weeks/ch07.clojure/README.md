# Getting Started

```sh
$ bash bootstrap.sh
# in a terminal:
$ ./software/bin/bake run-repl
# in a separate terminal:
$ ./software/bin/bake run-emacs
```

Then from within emacs:
```txt
M-x cider-connect
localhost
4001
```

Some useful keybindings:

```txt
C-x C-c                Quit Emacs
C-x C-s                Save The Current File
C-h ?                  General Help
C-h m                  Help on the current mode (eg: Cider/Clojure functionality)
M-x <<whatever>>       Run _any_ emacs function

# Clojure/Cider:
C-c C-k                Compile Current File
C-c M-i                Open the Inspector on the form to the left of the cursor.
```
