object SetUtil {
  def intersection( left: Seq[String], right: Seq[String]) : Seq[String] = {
    var seen = Map[String,Int]()
    for ( entry <- left ) {
      seen = seen(entry) = 1 + seen.getOrElse (entry,0)
      printf("entry[%s]:%d\n",entry,seen(entry))
    }

    var result = List[String]()
    for ( entry <- right ) {
      if(seen.getOrElse(entry,0) > 0) {
        seen(entry) = seen(entry) - 1
        result = entry :: result
      }
    }

    result.reverse
  }
}

var left = List("a","a","b","c")
var right = List("a","b","c","d")
println(SetUtil.intersection(left,right))


var left = List("a","a","a","b","c")
var right = List("a","a","b","c","d")
println(SetUtil.intersection(left,right))

