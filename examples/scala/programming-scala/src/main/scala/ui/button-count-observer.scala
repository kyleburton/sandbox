package ui
import observer._

class ButtonCountObserver {
  var count = 0
  def receiveUpdate(subject: Any) {
    count += 1
    println("ButtonCountObserver.receiveUpdate: subject=" + subject + ", count=" + count )
  }
}
