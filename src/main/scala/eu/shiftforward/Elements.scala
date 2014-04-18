package eu.shiftforward

trait LogicElements extends CircuitSimulation {
  def inverter(input: Wire) = {
    val output = new Wire
    input addAction { () =>
      val inputSig = input.getSignal
      schedule(InverterDelay) { output ~> !inputSig }
    }
    output
  }

  def connect(input: Wire, output: Wire) {
    input addAction { () =>
      val inputSig = input.getSignal
      schedule(InverterDelay) { output ~> inputSig }
    }
  }

  def associativeLogicGate(ins: List[Wire])(op: (Boolean, Boolean) => Boolean) = {
    val output = new Wire
    def action() {
      val inputs = ins.map(_.getSignal)
      schedule(GenericGateDelay) { output ~> inputs.reduceLeft(op) }
    }

    ins foreach { _ addAction action }
    output
  }

  def binaryLogicGate(a: Wire, b: Wire)(op: (Boolean, Boolean) => Boolean) = {
    val output = new Wire
    def action() {
      val inputA = a.getSignal
      val inputB = b.getSignal
      schedule(GenericGateDelay) { output ~> op(inputA, inputB) }
    }

    a addAction action
    b addAction action
    output
  }

  def and(ins: List[Wire])   = associativeLogicGate(ins) { _ && _ }
  def or(ins: List[Wire])    = associativeLogicGate(ins) { _ || _ }
  def xor(ins: List[Wire])   = associativeLogicGate(ins) { _ ^ _ }
  def nand(ins: List[Wire])  = associativeLogicGate(ins) { (x, y) => !(x && y) }
  def nor(ins: List[Wire])   = associativeLogicGate(ins) { (x, y) => !(x || y) }

  def and(a: Wire, b: Wire)  = binaryLogicGate(a, b) { _ && _ }
  def or(a: Wire, b: Wire)   = binaryLogicGate(a, b) { _ || _ }
  def xor(a: Wire, b: Wire)  = binaryLogicGate(a, b) { _ ^ _ }
  def nand(a: Wire, b: Wire) = binaryLogicGate(a, b) { (x, y) => !(x && y) }
  def nor(a: Wire, b: Wire)  = binaryLogicGate(a, b) { (x, y) => !(x || y) }
}

trait SequentialElements extends CircuitSimulation {
  def clock(out: Wire, interval: Int = 1, signal: Boolean = false) {
    schedule(interval) {
      out ~> !signal
      clock(out, interval, !signal)
    }
  }

  def flipflop(set: Wire, reset: Wire, initState: Boolean = false) = {
    val out, cout = new Wire
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

    (out, cout)
  }
}

trait ArithmeticElements extends LogicElements {
  def halfAdder(a: Wire, b: Wire) = {
    val c = and(a, b)
    (and(or(a, b), inverter(c)), c)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire) = {
    val (s,   c1) = halfAdder(a, cin)
    val (sum, c2) = halfAdder(b, s)
    (sum, or(c1, c2))
  }

  def multiBitAdder(a: Bus, b: Bus) = {
    val sum = new Bus(a.size)
    val overflow = new Wire

    connect((a zip b zip sum).foldLeft(Ground: Wire) { case (carry, ((a, b), c)) =>
      val (c1, cout) = fullAdder(a, b, carry)
      connect(c1, c)
      cout
    }, overflow)

    (sum, overflow)
  }

  def multiBitIncrementer(a: Bus) =
    multiBitAdder(a, constant8(a.size)(0x01))

  lazy val constant8 = (width: Int) => (c: Int) => {
    val bus = new Bus(width)
    bus.setSignal(c)
    bus
  }
}

trait ControlFlowElements extends LogicElements {
  def mux(a: Wire, b: Wire, s: Wire) =
    or(and(a, inverter(s)), and(b, s))

  def multiBitMultiplexer(a: Bus, b: Bus, selector: Wire) =
    new Bus((a zip b).map { case (a, b) => mux(a, b, selector) })

  def demux(a: Wire, s: Wire) =
    (and(a, inverter(s)), and(a, s))
}