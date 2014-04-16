package eu.shiftforward

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

trait OptimizedControlFlowElements extends ControlFlowElements {
  override def mux(a: Wire, b: Wire, s: Wire, output: Wire) {
    def action() {
      val inputA = a.getSignal
      val inputB = b.getSignal
      val selector = s.getSignal
      schedule(GenericGateDelay) {
        output ~> (if (selector) inputB else inputA)
      }
    }

    a addAction action
    b addAction action
    s addAction action
  }

  override def demux(a: Wire, s: Wire, outA: Wire, outB: Wire) {
    def action() {
      val input = a.getSignal
      val selector = s.getSignal
      schedule(GenericGateDelay) {
        outA ~> (if (!selector) input else false)
        outB ~> (if (selector)  input else false)
      }
    }

    a addAction action
    s addAction action
  }
}

trait OptimizedArithmeticElements extends ArithmeticElements {
  override def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) {
    def action() {
      val (outSum, outCarry) = (a.getSignal, b.getSignal, cin.getSignal) match {
        case (false, false, false) => (false, false)
        case (false, false, true)  => (true,  false)
        case (false,  true, false) => (true,  false)
        case (false,  true, true)  => (false,  true)
        case (true,  false, false) => (true,  false)
        case (true,  false, true)  => (false,  true)
        case (true,   true, false) => (false,  true)
        case (true,   true, true)  => (true,   true)
      }

      schedule(GenericGateDelay) {
        sum ~> outSum
        cout ~> outCarry
      }
    }

    a addAction action
    b addAction action
    cin addAction action
  }
}

trait OptimizedElements extends OptimizedControlFlowElements with OptimizedArithmeticElements