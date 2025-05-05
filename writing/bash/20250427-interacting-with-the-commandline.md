# OSX and the Mac Terminals

The Mac Terminal app doesn't support the Option key as a default for the ALT (Meta) key, though you can enable that in the Terminal's settings.  If you are on a Mac you should do this, in my opinion, you should do this now.  If you find yourself in a situation where you cannot do this (you are on someone else's machine), you can go old school, press and release `ESC` followed by the hotkey .. though really, please [Use Option as Meta Key](https://superuser.com/questions/1038947/using-the-option-key-properly-on-mac-terminal).

# Keybindings

Bash has two modes for keybindings: Emacs style and vim style.  It defaults to Emacs, though you can swap between them with `set -o vi` and set -o `emacs`.  As emacs is the default that's what I'll summarize here.  You can find documentation on the keybindings in the `bash` manpage (`man bash`), in the [Bash Reference Manual](software/bash/manual/bash.html), and [Bash Shortcuts](https://gist.github.com/tuxfight3r/60051ac67c5f0445efee) which is a nice summary.

You can see all of bound and unbound bindings in your current shell by running `bind -P`.  You can filter them for what is bound in your terminal with `grep`:

```bash
# commands bound to CTRL:
bind -P | grep '"\\C-[a-z]"'

# commands bound to ALT (aka META):
bind -P | grep '"\\e[a-z]"'
```
# Interacting with your shell history

You can see your current shell's history with the `history` command.  You can move up (or backwards) in the history with CTRL-p, you can move down (or forwards) in the history with CTRL-n.  You can search your history with CTRL-r ("reverse" search, because you are searching backwards towards the beginning of the history)

# Editing at the command line

```
$ git grep ClassFactory src/
               ^
               |               # if your cursor is at the second 's' in Class
              . <- CTRL-b      # backwards character
      CTRL-f -> .              # forwards character
           . <- ALT-b          # backwards word
              ALT-f -> .       # forwards word
  . <- CTRL-A                  # mnemonic: A is the beginning of the alphabet, jumps to the beginning of the line
                 CTRL-E -> .   # mnemonic: E is for End
```

# The value of staying in one place

When working at the terminal, the commands you enter will invariably reference files.  When specifying files and paths you have two choices: an absolute path or a relative path.  Relative pathing is less verbose and most often what we end up using.

```bash
# relatively pathed
bash ./my-task.sh 2>&1 | tee out.txt
bash my-task.sh 2>&1 | tee out.txt

# absolutely pathed
bash /home/kyle/my-task.sh
bash "$HOME/my-task.sh"
bash ~/my-task.sh
```

When we're working at a terminal we often change directory into a project or working directory before running our commands.  Using relative pathing and then changing directory will mean we have to "move back" to where a command from our history was run from in order for it to work unmodified.

```bash
... come up with a not so contrived example ...
```

# Moving Things around

Taking the output of one command and making it an augment to another command.  You can take the output of a command (or function) and place it onto the command line of another command with `$()`, which executes the command inside the parentheses and places it's output into the command line.

```bash
> which vim
/usr/bin/vim
> ls -lFa $(which vim)
lrwxrwxrwx 1 root root 21 Jan 16 12:13 /usr/bin/vim -> /etc/alternatives/vim*
```

`ls -lFa $(which vim)` has the same effect as typing `ls -lFa /usr/bin/vim`, where `/usr/bin/vim` was the output from `which vim`.
