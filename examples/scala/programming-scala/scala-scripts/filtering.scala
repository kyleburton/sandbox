val dogBreeds = List("Doberman", "Yorkshire Terrier", "Dachshund", "Scottish Terrier", "Great Dane", "Portuguese Water Dog")

for (breed <- dogBreeds )
  println(breed)

for (breed <- dogBreeds
   if breed.contains("Terrier"))
  println(breed)

for ( breed <- dogBreeds
   if breed.contains("Terrier");
   if !breed.contains("Yorkshire"))
  println(breed)


for { breed <- dogBreeds }
    println(breed)



val filterBreeds = for {
  breed <- dogBreeds
   if breed.contains("Terrier");
   if !breed.contains("Yorkshire")
} yield breed

filterBreeds
filterBreeds.first


for { breed <- dogBreeds
   upcasedBreed = breed.toUpperCase()
} println(upcasedBreed)



dogBreeds.map { b => b.toUpperCase() }

dogBreeds.map { breed => breed.toUpperCase() }


import java.util.Calendar

def isFridayThirteen ( cal: Calendar ): Boolean = {
  val dayOfWeek = cal.get(Calendar.DAY_OF_WEEK)
  val dayOfMonth = cal.get(Calendar.DAY_OF_MONTH)
  (dayOfWeek == Calendar.FRIDAY) && (dayOfMonth == 13)
}

// while ( ! isFridayThirteen(Calendar.getInstance()) ) {
//   println("Today isn't Friday the 13th.  Lame.")
//   Thread.sleep( 3600 * 24 * 1000 )
// }

var count = 0
do {
  count += 1
  println(count)
} while ( count < 10 )

for { i <- 1 to 10 }
    println(i)

val smallRange = 1 to 10

for { i <- smallRange }
    println(i)

var truncatedRange = 1 until 10
{
  println(smallRange)
  println(truncatedRange)
}

{
  val bools = List(true, false)
  for { bool <- bools }
      bool match {
        case true  => println("Heads")
        case false => println("tails")
        case _     => println("the edge?")
      }
}

import scala.util.Random

val randomInt = new Random().nextInt(10)


randomInt match {
  case 7 => println("lucky seven")
  case otherNumber => println("got a " + otherNumber)
}

def myRand ( ): Int = {
  new Random().nextInt(10)
}

myRand() match {
  case 7 => println("lucky seven")
  case otherNumber => println("got a " + otherNumber)
}



val sundries = List(23, "Hello", 8.5, 'q')

for { sundry <- sundries } {
  sundry match {
    case i: Int    => println("got an Integer: " + i)
    case s: String => println("got a  String:   " + s)
    case f: Double => println("got a  Double: " + f)
    case other     => println("got someting else: " + other)
  }
}




val willWork = List(1,3,23,90)
val willNotWork = List(4,18,52)
val empty = List()

for { l <- List(willWork, willNotWork, empty) } {
  l match {
    case List(_, 3, _, _) => println("four empty elements, with 2nd being a '3'.")
    case List(_*) => println("Any other list with 0 or more elements")
  }
}


def processList ( l: List[Any] ): Unit = {
  l match {
    case head :: tail =>
      format("%s ", head)
      processList(tail)
    case Nil => println("")
  }
}

{
  println("")
  for { l <- List(willWork,willNotWork,empty) } {
    print("List: ")
    processList(l)
  }
}

// val roman = Map( 1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V")
// roman


val tupA = ("Good", "Morning")
val tupB = ("Guten", "Tag!")

for (tup <- List(tupA, tupB)) {
  tup match {
    case (thingOne, thingTwo) if thingOne == "Good" =>
      println("A two-tuple starting with 'Good': '(" + thingOne + "," + thingTwo + ")'")
    case (thingOne, thingTwo) =>
      println("A two-tuple (no guard): '(" + thingOne + "," + thingTwo + ")'")
  }
}


case class Person (name: String, age: Int)e

val alice = new Person("Alice", 25)
val bob = new Person("Bob",32)
val charlie = new Person("Charlie", 32)

for (person <- List(alice, bob, charlie)) {
  person match {
    case p @ Person("Alice",25) => println("Alice: " + p)
    case p @ Person("Bob", 32)  => println("Bob: " + p)
    case p @ Person(name, age)  => println("Unknown: p=" + p + ", name=" + name + ", age=" + age)
  }
}


// pattern matching with regexes and binding

val bookExtractorRe = """Book: title=([^,]+),\s+authors=(.+)""".r
val magazineExtractorRe = """Magazine: title=([^,]+),\s+issue=(.+)""".r

val catalog = List(
  "Book: title=Programming Scala, authors=Dean Wampler, Alex Payne",
  "Magazine: title=The New Yorker, issue=January 2009",
  "Book: title=War and Peace, authors=Leo Tolstoy",
  "Magazine: title=The Atlantic, issue=February 2009",
  "BadData: text=Who put this here??"
)

for (item <- catalog ) {
  item match {
    case bookExtractorRe(title,authors) =>
      println("Book, title='" + title +"', authors='" + authors + "'")
    case magazineExtractorRe(title, issue) =>
      println("Magazine, title='" + title +"', issue='" + issue + "'")
    case entry =>
      println("Unrecognized entry: '" + entry + "'")
  }
}



