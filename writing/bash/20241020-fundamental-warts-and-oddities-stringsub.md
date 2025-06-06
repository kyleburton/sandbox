Bash's string substitution looked super terse and confusing to me for a long long time, though once I learned it I now use it frequently.

Want to strip a prefix or suffix from a variable? Want to strip the minimum or the maximum?  Use bash's string substitution.

There are two types of substitution: one to strip a prefix `#` and `##`, one to strip a suffix `%` and `%%`.  The two forms are "minimal" and "maximal" - `#` strips the minimum prefix while `##` strips the maximum prefix.  These prefixes support [bash's glob syntax](https://tldp.org/LDP/abs/html/globbingref.html), though you can also use hardcoded text.

In my opinion, this is best understood through examples and best learned by trying it out on your own.


```bash
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
```
