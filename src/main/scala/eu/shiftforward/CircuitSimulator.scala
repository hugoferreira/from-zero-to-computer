package eu.shiftforward

abstract class BasicCircuitSimulation extends Simulation {
  val GenericGateDelay: Int = 2
  val InverterDelay: Int = 1

  class Wire {
    private var signal = false
    private var actions: List[Action] = List()

    def getSignal = signal

    def setSignal(s: Boolean) {
      if (s != signal) {
        signal = s
        actions foreach (_())
      }
    }

    def addAction(a: Action) {
      actions ::= a
      a()
    }
  }

  def probe(name: String, wire: Wire) {
    wire addAction { () => println(name + " @ " + currentTime + " = " + wire.getSignal) }
  }

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

  def run(probes: List[(String, Wire)], cycles: Int = 1)(implicit tracer: Tracer = new ConsoleTracer) {
    val stopTime = currentTime + cycles
    while (hasNext && currentTime < stopTime) {
      next()
      tracer.trace(currentTime, probes.map(_._2.getSignal))
    }
  }
}

trait Tracer {
  def setHeader(probes: List[String])
  def trace(currentTime: Int, currentValues: List[Boolean])
}

class ConsoleTracer extends Tracer {
  var lastValues = List[Boolean]()

  def prettyPrintSignal(h: Boolean, s: Boolean) = (h, s) match {
    case (false, false) => "│  "
    case (false, true)  => "└─┐"
    case (true, true)   => "  │"
    case (true, false)  => "┌─┘"
  }

  def setHeader(probes: List[String]) {
    println("time\t" + probes.mkString("\t"))
  }

  def trace(currentTime: Int, currentValues: List[Boolean]) {
    val signals = if (!lastValues.isEmpty) lastValues.zip(currentValues).map { case (h, s) => prettyPrintSignal(h, s) }
                  else currentValues.map(s => prettyPrintSignal(s, s))
    println(currentTime + "\t" + signals.mkString("\t"))
    lastValues = currentValues
  }
}