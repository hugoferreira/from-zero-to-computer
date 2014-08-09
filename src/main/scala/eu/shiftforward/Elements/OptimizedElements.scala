package eu.shiftforward.Elements

import eu.shiftforward.{Bus, Wire}

import scala.collection.mutable.ArrayBuffer

trait OptimizedControlFlow extends ControlFlow {
  override def mux(a: Wire, b: Wire, s: Wire) = {
    val output = new Wire
    def action() {
      val inputA = a.getSignal
      val inputB = b.getSignal
      val selector = s.getSignal
      schedule(GenericGateDelay) {
        output <~ (if (selector) inputB else inputA)
      }
    }

    a addAction action
    b addAction action
    s addAction action

    output
  }

  override def demux(a: Wire, s: Wire) = {
    val outA, outB = new Wire

    def action() {
      val input = a.getSignal
      val selector = s.getSignal
      schedule(GenericGateDelay) {
        outA <~ (if (!selector) input else false)
        outB <~ (if (selector)  input else false)
      }
    }

    a addAction action
    s addAction action

    (outA, outB)
  }
}

trait OptimizedArithmetic extends Arithmetic {
  override def fullAdder(a: Wire, b: Wire, cin: Wire) = {
    val sum, cout = new Wire

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
        sum <~ outSum
        cout <~ outCarry
      }
    }

    a addAction action
    b addAction action
    cin addAction action

    (sum, cout)
  }
}

trait OptimizedMemory extends Memory {
  override def ram(data: Bus, address: Bus, load: Wire)(implicit clock: Wire): Bus = {
    val state = ArrayBuffer.fill(math.pow(2, address.size).toInt)(0)

    val out = new Bus(data.size)
    def action() {
      val clockIn = clock.getSignal
      val loadIn = load.getSignal
      val inputA = data.toInt
      schedule(ClockedGateDelay) {
        if (clockIn && loadIn) state(address.toInt) = inputA
        if (clockIn) out <~ state(address.toInt)
      }
    }

    clock addAction action

    out
  }
}

trait OptimizedElements extends OptimizedControlFlow with OptimizedArithmetic with OptimizedMemory