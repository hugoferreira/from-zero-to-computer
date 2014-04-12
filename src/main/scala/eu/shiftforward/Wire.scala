package eu.shiftforward

class Wire {
  private var signal = false
  private var actions: List[Simulation#Action] = List()

  def getSignal = signal

  def setSignal(s: Boolean) {
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
