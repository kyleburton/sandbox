package shapes {

  import scala.actors._
  import scala.actors.Actor._


  class Point(val x: Double, val y: Double) {
    override def toString() = "Point(" + x + "," + y +")"
  }

  abstract class Shape() {
    def draw(): Unit
  }

  class Circle(val center: Point, val radius: Double) extends Shape {
    def draw = println("Circle:draw: " + this)
    override def toString = "Circle(" + center + "," + radius + ")"
  }

  class Rectangle( val lowerLeft: Point, val height: Double, val width: Double ) extends Shape {
    def draw() = println("Rectangle.draw: " + this)
    override def toString = "Rectangle(" + lowerLeft + "," + height + "," + width + ")"
  }

  class Triangle(val point1: Point, val point2: Point, point3: Point) extends Shape {
    def draw() = println("Triangle.draw: " + this)
    override def toString() = "Triangle(" + point1 + "," + point2 + "," + point3 + ")"
  }

  object ShapeDrawingActor extends Actor {
    def act() = {
      var localVal = "[local value]"
      loop {
        var loopVal = "[loop value]"
        receive {
          case s: Shape => s.draw()
          case "exit"   => println(localVal + loopVal + "Exiting..."); exit
          case x: Any   => println(localVal + loopVal + "Error: Unknown message: '" + x + "'")
        }
      }
    }
  }



}
