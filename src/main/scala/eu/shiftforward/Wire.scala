package eu.shiftforward

trait Connector[T] {
  protected var signal: T
  protected var actions: List[Simulation#Action] = List()

  def getSignal: T = signal

  def setSignal(s: T) {
    if (s != signal) {
      signal = s
      actions foreach (_())
    }
  }

  def addAction(a: Simulation#Action) {
    actions ::= a
    a()
  }
}

class Wire extends Connector[Boolean] {
  var signal = false
}

class Bus8 extends Connector[Short] {
  var signal = 0: Short
}

object Ground extends Wire {
  override def getSignal = false
  override def setSignal(s: Boolean) { }
  override def addAction(a: Simulation#Action) { }
}

object Source extends Wire {
  override def getSignal = true
  override def setSignal(s: Boolean) { }
  override def addAction(a: Simulation#Action) { }
}
