package eu.shiftforward

trait Connector[T] {
  def getSignal: T
  def setSignal(s: T)
  def addAction(a: Simulation#Action)
}

class Wire extends Connector[Boolean] {
  var signal: Boolean = false
  var actions: List[Simulation#Action] = List()

  def getSignal: Boolean = signal

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

// ToDo: use shapeless to enforce width conformance at type level
class Bus(wires: Wire*) extends Connector[Iterable[Boolean]] with Iterable[Wire] {
  // from least to most significant
  def this(width: Int) = this((0 to width).map(_ => new Wire) : _*)

  def iterator: Iterator[Wire] = wires.iterator

  def getSignal = wires.map(_.getSignal)

  def setSignal(ss: Iterable[Boolean]) {
    wires.zip(ss).foreach { case (w, s) => w setSignal s }
  }

  def setSignal(s: Int) {
    wires.zip(s.toBinaryString.reverse).foreach {
      case (sig, '0') => sig setSignal false
      case (sig, '1') => sig setSignal true
    }
  }

  def addAction(a: Simulation#Action) {
    wires.foreach { _ addAction a }
  }

  override def toString() = wires.map(s => if (s.getSignal) 1 else 0).mkString
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
