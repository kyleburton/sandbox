package ui
import observer._

class ButtonCountObserver {
  var count = 0
  def receiveUpdate(subject: Any) {
    count += 1
  }
}
