package eu.shiftforward

import scala.Function.tupled

trait LogicElements extends CircuitSimulation {
  def inverter(input: Wire): Wire = {
    val output = new Wire
    input addAction { () =>
      val inputSig = input.getSignal
      schedule(InverterDelay) { output ~> !inputSig }
    }
    output
  }

  def inverter(input: Bus): Bus = new Bus(input map inverter)

  def connect(input: Wire, output: Wire) {
    input addAction { () =>
      val inputSig = input.getSignal
      schedule(InverterDelay) { output ~> inputSig }
    }
  }

  def connect(input: Bus, output: Bus) {
    input zip output foreach tupled { connect(_, _) }
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

  def and(ins: List[Wire]): Wire  = ins reduceLeft and
  def or(ins: List[Wire]): Wire   = ins reduceLeft or
  def xor(ins: List[Wire]): Wire  = ins reduceLeft xor
  def nand(ins: List[Wire]): Wire = ins reduceLeft nand
  def nor(ins: List[Wire]): Wire  = ins reduceLeft nor

  def and(a: Wire, b: Wire)  = binaryLogicGate(a, b) { _ && _ }
  def or(a: Wire, b: Wire)   = binaryLogicGate(a, b) { _ || _ }
  def xor(a: Wire, b: Wire)  = binaryLogicGate(a, b) { _ ^ _ }
  def nand(a: Wire, b: Wire) = binaryLogicGate(a, b) { (x, y) => !(x && y) }
  def nor(a: Wire, b: Wire)  = binaryLogicGate(a, b) { (x, y) => !(x || y) }

  def and(x: Bus, y: Bus): Bus  = new Bus(x zip y map tupled { and(_, _) })
  def or(x: Bus, y: Bus): Bus   = new Bus(x zip y map tupled { or(_, _) })
  def xor(x: Bus, y: Bus): Bus  = new Bus(x zip y map tupled { xor(_, _) })
  def nand(x: Bus, y: Bus): Bus = new Bus(x zip y map tupled { nand(_, _) })
  def nor(x: Bus, y: Bus): Bus  = new Bus(x zip y map tupled { nor(_, _) })
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
    val (wires, overflow) = (a zip b).foldLeft((List[Wire](), Ground: Wire)) {
      case ((bus, carry), (a, b)) =>
        val (c1, cout) = fullAdder(a, b, carry)
        (c1 :: bus, cout)
    }

    (new Bus(wires.reverse), overflow)
  }

  def multiBitIncrementer(a: Bus) =
    multiBitAdder(a, constant(a.size)(0x01))

  lazy val constant = (width: Int) => (c: Int) => {
    val bus = new Bus(width)
    bus.setSignal(c)
    bus
  }
}

trait ControlFlowElements extends LogicElements {
  def mux(a: Wire, b: Wire, s: Wire): Wire =
    or(and(a, inverter(s)), and(b, s))

  def mux(a: Bus, b: Bus, selector: Wire): Bus =
    new Bus(a zip b map tupled { mux(_, _, selector) })

  def demux(a: Wire, s: Wire) =
    (and(a, inverter(s)), and(a, s))
}