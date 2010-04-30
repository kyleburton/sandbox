// val foo : List[() => Unit] = null

package ui

class Button( val label : String ) extends Widget {
  def click() = {
    println("Button.click:" + this)
  }
}


