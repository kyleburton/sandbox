#!/bin/bash
########################################
file_name=/home/kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md
echo "file_name=$file_name"

# strip the shortest prefix up to '/'
echo "Strip up to the first /:    ${file_name#*/}"
# 'home/kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md'

# strip to the second '/'
echo "Strip up to the second /:   ${file_name#*/*/}"
# 'kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md'

# strip $HOME
echo "strip \$HOME:                ${file_name#"$HOME"/}"
# 'code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md'

# greedily strip up to the '/', aka `basename`
echo "strip up to the last /:     ${file_name##*/}"
# '20241020-fundamental-warts-and-oddities-stringsub.md'

# strip the shortest strip shortest suffix after '/', aka `dirname`
echo "strip shortest suffix starting with /:    ${file_name%/*}"
# '/home/kyle/code/github.com/kyleburton/sandbox/writing/bash'

# strip the longest suffix, two '/'s
echo "strip the longest suffix with two /:      ${file_name%/*/*}"
# '/home/kyle/code/github.com/kyleburton/sandbox/writing'

########################################
file_and_line="src/com/bigConcrete/AbstractSatisfactoryConcreteFactory.java:512"

echo "file_and_line=$file_and_line"
# greedily strip after ':', aka get the line number
echo "strip longest prefix up to the first ':'    ${file_and_line##*:}"
# '512'

# greedily strip up to ':', aka get the file name
echo "strip after the ':'                         ${file_and_line%%:*}"
# 'src/com/bigConcrete/AbstractSatisfactoryConcreteFactory.java'

# Strip the first four chars
echo "strip the first four chars:                 ${file_name#????}"
# 'e/kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md'

# Strip the last four chars
echo "strip the last four chars:                 ${file_name%????}"
