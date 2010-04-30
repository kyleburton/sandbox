object Breed extends Enumeration {
  val doberman = Value("Doberman Pinscher")
  val yorkie = Value("Yorkshire Terrier")
  val scottie = Value("Scottish Terrier")
  val dane = Value("Great Dane")
  val portie = Value("Portuguese Water Dog")
}

println("ID\tBreed")
for(breed <- Breed) println(breed.id + "\t" + breed)

println("\nJust Terriers")
Breed.filter(_.toString.endsWith("Terrier")).foreach(println)


object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}

import WeekDay._

def isWorkingDay( d: WeekDay ) = ! (d == Sat || d == Sun)

WeekDay filter isWorkingDay foreach println
