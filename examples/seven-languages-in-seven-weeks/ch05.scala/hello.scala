
import scala.language.postfixOps

class Compass {
  val directions = List("north", "east", "south", "west") 
  var bearing = 0
  print("Initial bearing: ")
  println(direction)
  def direction() = directions(bearing)
  def inform(turnDirection: String) {
    println("Turning " + turnDirection + ". Now bearing " + direction)
  }
  def turnRight() {
    bearing = (bearing + 1) % directions.size 
    inform("right")
  }
  def turnLeft() {
    bearing = (bearing + (directions.size - 1)) % directions.size 
    inform("left")
  } 
}


// static methods go in the 'singleton' object TrueRing, you can also
// have a class definition with the same name, where the instance
// methods go...IMO this is cleaner than straight java which mixes
// the static and non-static methods the same class definition...
object TrueRing {
  def rule = println("To rule them all")
}



object Hi {

  def forLoop(args: Array[_]) {
    for(i <- 0 until args.length ) {
      println(args(i))
    }
  }

  def forList(args: Array[_]) {
    args.foreach { arg =>
      println(arg)
    }
  }

  class Person1(firstName: String, lastName: String) {
    override
    def toString() : String = {
      String.format("Person1[%s]{firstName=%s;lastName=%s}", hashCode() toString, firstName, lastName)
    }
  }

  class Person(name: String) {
    def talk(message: String) = println(name + " says " + message)
  }

  trait Nice {
    def greet() = println("Howdily doodily.")
  }

  class Character(val name: String) extends Person(name) with Nice

  class Employee(val name: String, val number: Int) extends Person(name) {
    override def talk(message: String) {
      println(name + " with number " + number + " says " + message)
    }

    def id(): String = number.toString()
  }


  def _main(args: Array[String]) {
    println("hi Scala and ensime!")
    forLoop(Array("this","that","other"))
    forList(Array("this","that","other"))

    val range = 0 until 10
    printf("range=%s\n", range)
    printf("range.start=%s\n", range.start)
    printf("range.end=%s\n", range.end)
    printf("range.step=%s\n", range.step)
    val r2 = 0 until 10 by 3
    printf("r2=%s\n", r2)
    printf("r2.start=%s\n", r2.start)
    printf("r2.end=%s\n", r2.end)
    printf("r2.step=%s\n", r2.step)
    r2.foreach { ii =>
      printf("ii=%s\n", ii)
    }

    printf("[1 10] 1 to 10 => %s\n", 1 to 10 toList)
    printf("[1 10) 1 until 10 => %s\n", 1 until 10 toList)

    printf("a to z => %s\n", 'a' to 'z')
    printf("a to Z => %s\n", 'a' to 'Z')
    printf("A to Z => %s\n", 'A' to 'Z')

    val person = ("Elvis", "Presley")
    printf("person=%s\n", person)
    printf("person._1=%s\n", person._1)
    // NB: scala can detect that _3 is not a member, _at_compile_time_ this is a WIN
    // printf("person._3=%s\n", person._3)

    val gump = new Person1("Forest", "Gump")
    printf("gump=%s\n", gump)

    val myCompass = new Compass()
    myCompass.turnRight
    myCompass.turnRight
    myCompass.turnLeft
    myCompass.turnLeft
    myCompass.turnLeft

    TrueRing.rule

    val employee = new Employee("Yoda", 4)
    employee.talk("Extend or extend not.  There is no try.")

    val flanders = new Character("Ned")
    print("Flanders' greets you like this: ")
    flanders.greet
  }

}

