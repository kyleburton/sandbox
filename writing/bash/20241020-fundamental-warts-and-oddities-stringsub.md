Bash's string substitution looked super terse and confusing to me for a long long time, though once I learned it, it became one of my most commonly used idioms.


Want to strip a prefix or suffix from a variable? Want to strip the minimum or the maximum?  Use bash's string substitution.

There are two types of substitution: one to strip a prefix `#` and `##`, one to strip a suffix `%` and `%%`.  The two forms are "minimal" and "maximal" - `#` strips the minimum prefix while `##` strips the maximum prefix.  These prefixes support literal characters as well as [bash's glob syntax](https://tldp.org/LDP/abs/html/globbingref.html).

In my opinion, this is best understood through examples and best learned by trying it out on your own.

```bash
########################################
file_name=/home/kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md

# strip the shortest prefix up to '/'
echo "${file_name#*/}"
# 'home/kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md'

# strip to the second '/'
echo "${file_name#*/}"
# 'kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md'

# strip $HOME
echo "'${file_name#"$HOME"/}'"
# 'code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md'

# greedily strip up to the '/', aka `basename`
echo "${file_name##*/}"
# '20241020-fundamental-warts-and-oddities-stringsub.md'

# strip the shortest strip shortest suffix after '/', aka `dirname`
echo "${file_name%/*}"
# '/home/kyle/code/github.com/kyleburton/sandbox/writing/bash'

# strip the longest suffix, two '/'s
echo "${file_name%/*/*}"
# '/home/kyle/code/github.com/kyleburton/sandbox/writing'

########################################
file_and_line="src/com/bigConcrete/AbstractSatisfactoryConcreteFactory.java:512"

# greedily strip after ':', aka get the line number
echo "${file_and_line##*:}"
# '512'

# greedily strip up to ':', aka get the file name
# 'src/com/bigConcrete/AbstractSatisfactoryConcreteFactory.java'

# Strip the first four chars
echo "${file_name#????}"
# 'e/kyle/code/github.com/kyleburton/sandbox/writing/bash/20241020-fundamental-warts-and-oddities-stringsub.md'

```
