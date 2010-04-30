package ui
import org.specs._
import observer._

class ButtonObserverAnonSpec extends Specification {
  "A Button Observer" should {

    "observe button clicks" in {
      val observableButton = new Button("Okay") with Subject {
        override def click () = {
          super.click()
          notifyObservers
        }
      }

      val buttonObserver = new ButtonCountObserver
      observableButton.addObserver(buttonObserver)

      for ( i <- 1 to 3 ) observableButton.click()

      buttonObserver.count mustEqual 3
    }
  }

}
