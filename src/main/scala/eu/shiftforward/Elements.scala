package eu.shiftforward

import scala.Function.tupled

trait LogicElements extends CircuitSimulation {
  def inverter(input: Wire): Wire = {
    val output = new Wire
    input addAction { () =>
      val inputSig = input.getSignal
      schedule(InverterDelay) { output <~ !inputSig }
    }
    output
  }

  def inverter(input: Bus): Bus = input map inverter

  def binaryLogicGate(a: Wire, b: Wire)(op: (Boolean, Boolean) => Boolean) = {
    val output = new Wire
    def action() {
      val inputA = a.getSignal
      val inputB = b.getSignal
      schedule(GenericGateDelay) { output <~ op(inputA, inputB) }
    }

    a addAction action
    b addAction action
    output
  }

  def and(ins: Iterable[Wire]): Wire  = ins reduceLeft and
  def or(ins: Iterable[Wire]): Wire   = ins reduceLeft or
  def xor(ins: Iterable[Wire]): Wire  = ins reduceLeft xor
  def nand(ins: Iterable[Wire]): Wire = ins reduceLeft nand
  def nor(ins: Iterable[Wire]): Wire  = ins reduceLeft nor

  def and(a: Wire, b: Wire)  = binaryLogicGate(a, b) { _ && _ }
  def or(a: Wire, b: Wire)   = binaryLogicGate(a, b) { _ || _ }
  def xor(a: Wire, b: Wire)  = binaryLogicGate(a, b) { _ ^ _ }
  def nand(a: Wire, b: Wire) = binaryLogicGate(a, b) { (x, y) => !(x && y) }
  def nor(a: Wire, b: Wire)  = binaryLogicGate(a, b) { (x, y) => !(x || y) }

  def and(x: Bus, y: Bus): Bus  = (x, y).zipped map and
  def or(x: Bus, y: Bus): Bus   = (x, y).zipped map or
  def xor(x: Bus, y: Bus): Bus  = (x, y).zipped map xor
  def nand(x: Bus, y: Bus): Bus = (x, y).zipped map nand
  def nor(x: Bus, y: Bus): Bus  = (x, y).zipped map nor
}

trait SequentialElements extends CircuitSimulation {
  def clock(interval: Int = 1, signal: Boolean = false) = {
    val out = new Wire
    schedule(0) {
      out <~ signal
      ticktack(out, interval, signal)
    }
    out
  }

  private def ticktack(out: Wire, interval: Int = 1, signal: Boolean = false) {
    schedule(interval) {
      out <~ signal
      ticktack(out, interval, !signal)
    }
  }

  def dff(in: Wire, initState: Boolean = false)(implicit clock: Wire) = {
    var state = initState
    val out = new Wire

    def action() {
      if (clock.getSignal) {
        val inputA = in.getSignal
        out <~ state
        state = inputA
      }
    }

    clock addAction action

    out
  }

  def flipflop(set: Wire, reset: Wire, initState: Boolean = false) = {
    val out, cout = new Wire
    var state = initState

    def action() {
      val isSet = set.getSignal
      val isReset = reset.getSignal

      schedule(FlipFlopDelay) {
        state = isSet || (state && !isReset)

        out  <~ state
        cout <~ !state
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

  def multiBitAdder(a: Bus, b: Bus): (Bus, Wire) = {
    val (wires, overflow) = (a zip b).foldLeft((List[Wire](), Ground: Wire)) {
      case ((bus, carry), (a, b)) =>
        val (c1, cout) = fullAdder(a, b, carry)
        (bus :+ c1, cout)
    }

    (wires, overflow)
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
    (a, b).zipped map { mux(_, _, selector) }

  def demux(a: Wire, s: Wire) =
    (and(a, inverter(s)), and(a, s))
}

trait Memory extends SequentialElements with ControlFlowElements {
  def register(in: Wire, load: Wire)(implicit clock: Wire) = {
    val muxOut = new Wire
    val out = dff(muxOut)
    mux(out, in, load) ~> muxOut
    muxOut
  }

  def register(in: Bus, load: Wire)(implicit clock: Wire): Bus = in map { register(_, load) }
}