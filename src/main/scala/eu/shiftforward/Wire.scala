package eu.shiftforward

sealed trait Connector[T] {
  def getSignal: T
  def <~(s: T) = setSignal(s)
  def setSignal(s: T)
  def addAction(a: Simulation#Action)
}

class Wire extends Connector[Boolean] {
  private var signal: Boolean = false
  private var actions: List[Simulation#Action] = List()

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

  def connectTo(w: Wire) { addAction { () => w <~ getSignal } }
  def ~>(w: Wire) { connectTo(w) }
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

// ToDo: use shapeless to enforce width conformance at type level
class Bus(wires: Wire*) extends Connector[Iterable[Boolean]] with Seq[Wire] {
  def this(width: Int) = this((1 to width).map(_ => new Wire) : _*)
  def this(wires: Iterable[Wire]) = this(wires.toSeq : _*)

  def iterator: Iterator[Wire] = wires.iterator
  def length = wires.length
  def apply(idx: Int) = wires(idx)

  def getSignal = wires.map(_.getSignal)

  def setSignal(ss: Iterable[Boolean]) {
    (wires, ss).zipped foreach { _ <~ _ }
  }

  def setSignal(s: Int) {
    wires zip s.toBinaryString.reverse foreach {
      case (sig, c) => sig <~ (c == '1')
    }
  }

  def addAction(a: Simulation#Action) {
    wires foreach { _ addAction a }
  }

  def connectTo(b: Bus) { (this, b).zipped foreach { _ ~> _ } }
  def ~>(w: Bus) { connectTo(w) }

  override def toString() = wires.map(s => if (s.getSignal) 1 else 0).mkString.reverse
}
