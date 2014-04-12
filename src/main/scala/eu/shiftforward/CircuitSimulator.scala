package eu.shiftforward

abstract class CircuitSimulation extends Simulation {
  val GenericGateDelay: Int = 1
  val InverterDelay: Int = 1
  val FlipFlopDelay: Int = 1

  def debug(name: String, wire: Wire) {
    wire addAction { () => println(name + " @ " + currentTime + " = " + wire.getSignal) }
  }

  def run(cycles: Int = 1)(implicit tracer: Tracer = DummyTracer, probes: List[(String, Wire)]) {
    val stopTime = currentTime + cycles
    while (hasNext && currentTime < stopTime) {
      next()
      tracer.trace(currentTime, probes.map(_._2.getSignal))
    }

    curtime = stopTime
  }
}

trait SequentialElements extends CircuitSimulation {
  def clock(out: Wire, interval: Int = 1, signal: Boolean = false) {
    schedule(interval) {
      out setSignal !signal
      clock(out, interval, !signal)
    }
  }

  def flipflop(set: Wire, reset: Wire, out: Wire, cout: Wire, initState: Boolean = false) {
    var state = initState

    def action() {
      val isSet = set.getSignal
      val isReset = reset.getSignal

      schedule(FlipFlopDelay) {
        state = (state, isSet, isReset) match {
          case (false, false, false) => false
          case (false, false, true)  => false
          case (false,  true, false) => true
          case (false,  true, true)  => true
          case (true,  false, false) => true
          case (true,  false, true)  => false
          case (true,   true, false) => true
          case (true,   true, true)  => true
        }

        out  setSignal state
        cout setSignal !state
      }
    }

    set addAction action
    reset addAction action
  }
}

trait LogicElements extends CircuitSimulation {
  def inverter(input: Wire, output: Wire) {
    def action() {
      val inputSig = input.getSignal
      schedule(InverterDelay) { output setSignal !inputSig }
    }

    input addAction action
  }

  def associativeLogicGate(ins: List[Wire], output: Wire)(op: (Boolean, Boolean) => Boolean) {
    def action() {
      val inputs = ins.map(_.getSignal)
      schedule(GenericGateDelay) { output setSignal inputs.reduceLeft(op) }
    }

    ins.foreach(_ addAction action)
  }

  def binaryLogicGate(a: Wire, b: Wire, output: Wire)(op: (Boolean, Boolean) => Boolean) {
    def action() {
      val inputA = a.getSignal
      val inputB = b.getSignal
      schedule(GenericGateDelay) { output setSignal op(inputA, inputB) }
    }

    a addAction action
    b addAction action
  }

  def and(ins: List[Wire], output: Wire)   = associativeLogicGate(ins, output) { _ && _ }
  def or(ins: List[Wire], output: Wire)    = associativeLogicGate(ins, output) { _ || _ }
  def xor(ins: List[Wire], output: Wire)   = associativeLogicGate(ins, output) { _ ^ _ }
  def nand(ins: List[Wire], output: Wire)  = associativeLogicGate(ins, output) { (x, y) => !(x && y) }

  def and(a: Wire, b: Wire, output: Wire)  = binaryLogicGate(a, b, output) { _ && _ }
  def or(a: Wire, b: Wire, output: Wire)   = binaryLogicGate(a, b, output) { _ || _ }
  def xor(a: Wire, b: Wire, output: Wire)  = binaryLogicGate(a, b, output) { _ ^ _ }
  def nand(a: Wire, b: Wire, output: Wire) = binaryLogicGate(a, b, output) { (x, y) => !(x && y) }
}