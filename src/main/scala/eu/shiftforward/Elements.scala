package eu.shiftforward

trait LogicElements extends CircuitSimulation {
  def inverter(input: Wire, output: Wire) {
    input addAction { () =>
      val inputSig = input.getSignal
      schedule(InverterDelay) { output ~> !inputSig }
    }
  }

  def connect(input: Wire, output: Wire) {
    input addAction { () =>
      val inputSig = input.getSignal
      schedule(InverterDelay) { output ~> inputSig }
    }
  }

  def associativeLogicGate(ins: List[Wire], output: Wire)(op: (Boolean, Boolean) => Boolean) {
    def action() {
      val inputs = ins.map(_.getSignal)
      schedule(GenericGateDelay) { output ~> inputs.reduceLeft(op) }
    }

    ins foreach { _ addAction action }
  }

  def binaryLogicGate(a: Wire, b: Wire, output: Wire)(op: (Boolean, Boolean) => Boolean) {
    def action() {
      val inputA = a.getSignal
      val inputB = b.getSignal
      schedule(GenericGateDelay) { output ~> op(inputA, inputB) }
    }

    a addAction action
    b addAction action
  }

  def and(ins: List[Wire], output: Wire)   = associativeLogicGate(ins, output) { _ && _ }
  def or(ins: List[Wire], output: Wire)    = associativeLogicGate(ins, output) { _ || _ }
  def xor(ins: List[Wire], output: Wire)   = associativeLogicGate(ins, output) { _ ^ _ }
  def nand(ins: List[Wire], output: Wire)  = associativeLogicGate(ins, output) { (x, y) => !(x && y) }
  def nor(ins: List[Wire], output: Wire)   = associativeLogicGate(ins, output) { (x, y) => !(x || y) }

  def and(a: Wire, b: Wire, output: Wire)  = binaryLogicGate(a, b, output) { _ && _ }
  def or(a: Wire, b: Wire, output: Wire)   = binaryLogicGate(a, b, output) { _ || _ }
  def xor(a: Wire, b: Wire, output: Wire)  = binaryLogicGate(a, b, output) { _ ^ _ }
  def nand(a: Wire, b: Wire, output: Wire) = binaryLogicGate(a, b, output) { (x, y) => !(x && y) }
  def nor(a: Wire, b: Wire, output: Wire)  = binaryLogicGate(a, b, output) { (x, y) => !(x || y) }
}

trait SequentialElements extends CircuitSimulation {
  def clock(out: Wire, interval: Int = 1, signal: Boolean = false) {
    schedule(interval) {
      out ~> !signal
      clock(out, interval, !signal)
    }
  }

  def flipflop(set: Wire, reset: Wire, out: Wire, cout: Wire, initState: Boolean = false) {
    var state = initState

    def action() {
      val isSet = set.getSignal
      val isReset = reset.getSignal

      schedule(FlipFlopDelay) {
        state = isSet || (state && !isReset)

        out  ~> state
        cout ~> !state
      }
    }

    set addAction action
    reset addAction action
  }
}

trait ArithmeticElements extends CircuitSimulation with LogicElements {
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) {
    val d, e = new Wire
    or(a, b, d)
    and(a, b, c)
    inverter(c, e)
    and(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) {
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    or(c1, c2, cout)
  }

  def multiBitAdder(a: Bus, b: Bus, sum: Bus, overflow: Wire) {
    connect((a zip b zip sum).foldLeft(Ground: Wire) { case (carry, ((a, b), c)) =>
      val cout = new Wire
      fullAdder(a, b, carry, c, cout)
      cout
    }, overflow)
  }
}

trait ControlFlowElements extends CircuitSimulation with LogicElements {
  def mux(a: Wire, b: Wire, s: Wire, output: Wire) {
    val notS, outA, outB = new Wire

    inverter(s, notS)
    and(a, notS, outA)
    and(b, s, outB)
    or(outA, outB, output)
  }

  def multiBitMultiplexer(a: Bus, b: Bus, selector: Wire, out: Bus) {
    (a zip b zip out).map { case ((a, b), out) => mux(a, b, selector, out) }
  }

  def demux(a: Wire, s: Wire, outA: Wire, outB: Wire) {
    val notS, outA, outB = new Wire

    inverter(s, notS)
    and(a, notS, outA)
    and(a, s, outB)
  }
}