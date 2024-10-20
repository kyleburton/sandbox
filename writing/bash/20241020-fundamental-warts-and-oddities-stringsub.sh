file_name=/home/kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md

echo "file_name:                                    '$file_name'"
echo "file_name: strip shortest prefix up to '/':   '${file_name#*/}'"
echo "file_name: ... to the second '/':             '${file_name#*/*/}'"
echo "file_name: strip \$HOME/:                      '${file_name#"$HOME"/}'"
echo "file_name: greedy strip to '/'                '${file_name##*/}'"
echo "           equivalent to the basename command"
echo "file_name: strip shortest suffix after '/'    '${file_name%/*}'"
echo "           equivalent to the dirname command"
echo "file_name: ... to the second '/'              '${file_name%/*/*}'"
echo "file_name: greedy strip after '/'             '${file_name%%/*}'"

file_and_line="src/com/bigConcrete/AbstractSatisfactoryConcreteFactory.java:512"
echo "file_and_line:                                '${file_and_line}'"
echo "file_and_line: greedy strip after ':'         '${file_and_line##*:}'"
echo "file_and_line: aka get the line number"
echo "file_and_line: greedy strip up to ':'         '${file_and_line%%:*}'"
echo "file_and_line: aka get the file name"


echo "Pattern matching uses bash's glob matching: *, ?"
echo "Strip the first four chars:"
echo "${file_name#????}"
