package eu.shiftforward

abstract class CircuitSimulation extends Simulation {
  val GenericGateDelay: Int = 0
  val ClockedGateDelay: Int = 0
  val InverterDelay: Int = 0
  val FlipFlopDelay: Int = 0

  def debug[T](name: String, connector: Connector[T]) {
    connector addAction { () => println(name + " @ " + currentTime + " = " + connector.getSignal) }
  }

  def run(cycles: Int = 1)(implicit tracer: Tracer = DummyTracer) {
    val stopTime = currentTime + cycles
    while (hasNext && currentTime < stopTime) {
      step()
      tracer.trace(currentTime)
    }

    curtime = stopTime
  }

  implicit def toBus(wires: Iterable[Wire]): Bus = new Bus(wires)
  implicit def toWire(value: Boolean): Wire = if (value) Source else Ground
}