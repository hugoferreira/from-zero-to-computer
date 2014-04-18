package eu.shiftforward

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