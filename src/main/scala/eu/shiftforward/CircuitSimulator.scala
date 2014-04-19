package eu.shiftforward

abstract class CircuitSimulation extends Simulation {
  val GenericGateDelay: Int = 0
  val InverterDelay: Int = 0
  val FlipFlopDelay: Int = 0

  def debug[T](name: String, connector: Connector[T]) {
    connector addAction { () => println(name + " @ " + currentTime + " = " + connector.getSignal) }
  }

  def run(cycles: Int = 1)(implicit tracer: Tracer = DummyTracer) {
    val stopTime = currentTime + cycles
    while (hasNext && currentTime < stopTime) {
      next()
      tracer.trace(currentTime)
    }

    curtime = stopTime
  }

  implicit def toBus(wires: Iterable[Wire]) = new Bus(wires)
}