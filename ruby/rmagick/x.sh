 trap "echo sig1"  1
 trap "echo sig2"  2
 trap "echo sig3"  3
 trap "echo sig13" 13
 trap "echo sig15" 15

 read "ok, hitme"
