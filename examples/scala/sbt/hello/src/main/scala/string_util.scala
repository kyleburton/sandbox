object StringUtil {

  def joiner( strings: List[String], separator: String) : String =
    strings.mkString(separator)

  def joiner(strings: List[String]): String = strings.mkString(" ")
  def toCollection(string: String) = string.split(' ')
}

import StringUtil._
object StringUtilClient {
  def main ( args: Array[String]) = {
    println("foof")
    args foreach { s => toCollection(s).foreach {  x => println(x) } }
  }
}
