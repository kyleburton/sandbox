// Write a game that will take a tic-tac-toe board with X, O, and
// blank characters and detect the winner or whether there is a tie or
// no winner yet. Use classes where appropriate.
// Run with: 
import scala.collection.mutable.HashMap
import scala.collection.immutable.Set
import scala.collection.immutable.{HashMap => ImmutableMap}

class Board(board: String)

object App {
  def _main( args: Array[String] ) {
    println("Test your board here")
    printf("this: %s\n", List(1,2,3))
    printf("this: %s\n", List("one", "two", "three"))
    printf("this: %s\n", List("one", "two", "three", 4))

    val list = List("one", "two", 3)
    printf("list=%s, list(2)=%s\n", list, list(2))
    // printf("list(4)=%s\n", list(4))
    // => IOB Exception

    // so nice...oh NMIND, newer versions of scala throw an IOB Exception
    // printf("list(-1)=%s\n", list(-1))
    //   printf("list(-2)=%s\n", list(-2))
    //   printf("list(-3)=%s\n", list(-3))

    printf("Nil => %s", Nil)

    val animals = Set("lions", "tigers", "bears")
    printf("animals=%s\n", animals)

    var animals2 = animals + "armadillos"
    printf("animals2=%s\n", animals2)

    printf("animals2 - \"tigers\"=%s\n", animals2 - "tigers")

    // why not support logical operations on sets?
    // var animals3 = animals2 + Set("armadillos", "racoons")
    // printf("animals2 + Set(\"armadillos\", \"racoons\")=%s\n", animals3)
    // => compiler error

    // why a ++ instead of just +?
    printf("animals2 + Set(\"armadillos\", \"racoons\")=%s\n", animals2 ++ Set("armadillos", "racoons"))

    // "foo"

    val nums = Map(0 -> "zero", 1 -> "one", 2 -> "two")
    printf("nums=%s\n", nums)

    printf("nums(2)=%s\n", nums(2))

    val map = new HashMap[Int, String]
    map += 4 -> "four"
    map += 8 -> "eight"
    printf("map=%s\n", map)

    runafoo((x: Any) => printf("lambda: x=%s\n", x))

    val hobbits = List("frodo", "samwise", "pippin")
    val res = hobbits.foreach(h => printf("  hobbit=%s\n", h))
    printf("foreach's res=%s %s\n", res, res.getClass)

    val hobbits2 = Set("frodo", "samwise", "pippin")
    hobbits2.foreach(h => printf("  hobbit=%s\n", h))

    // simple closure
    val c = () => { println("Hello") }
    c()
    var anX = 0
    val badfn = () => { anX += 1 }
    printf("anX=%s\n", anX)
    badfn()
    printf("anX=%s\n", anX)
    badfn()
    printf("anX=%s\n", anX)
    // :(

    val strings = List("apple","banana","orange","grapefruit","kiwi","muttfruit","blueberry")
    val totalLen = strings.foldLeft(0)( (acc,s) => s.length + acc )
    printf("totalLen=%s\n", totalLen)

    // NB: just using fromFile.mkString leaks the file handle (doesn't close it)
    // val data = io.Source.fromFile("day1.scala").mkString
    // val slurp = (fname: String) : (String => String)  {
    //   try {
    //     val infh = io.Source.fromFile(fname)
    //     val data : String = infh.mkString
    //     infh.close()
    //     return data
    //   }
    //   catch {
    //     case e: Exception => 
    //       return nil
    //   }
    // }
    // val data = slurp("day1.scala")
    // printf("data.length=%s\n", data.length)
  }

  def runafoo(callback: (Any) => Any) {
    callback("runafoo")
  }
}
