/*
object Hi {
  def main(args: Array[String]) {
    println("Hi!")
    println("There!")
    println("You!")
  }
}

class Upper {
  def upper(strings: String*): Seq[String] = {
    strings.map((s:String) => s.toUpperCase())
    strings
  }

}

val up = new Upper
Console.println(up.upper("A", "First", "Scala", "program"))

*/

/*

object Upper {
  def upper(strings: String*) = strings.map(_.toUpperCase())
}

Console.println(Upper.upper("A","First","Scala","Program"))

*/
