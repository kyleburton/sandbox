package observer

trait Subject {
  type Observer = {  def receiveUpdate(subject: Any) }

  private var observers = List[Observer]()

  def addObserver(observer: Observer) = observers ::= observer
  def notifyObservers = observers foreach (_.receiveUpdate(this))
}

import Subject.Observer


