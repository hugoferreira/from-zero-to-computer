package eu.shiftforward

object DffTest extends App {
  new CircuitSimulation with SequentialElements {
    implicit val clk = clock(1)
    val in = new Wire
    val out = dff(in)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("in", in), ("out", out), ("clk", clk))

    run(3)

    in <~ true
    run(3)

    in <~ false
    run(3)

    in <~ true
    run(3)

    in <~ false
    run(3)
  }
}

object RegisterTest extends App {
  new CircuitSimulation with Memory {
    implicit val clk = clock(1)
    val in, load = new Wire
    val out = register(in, load)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("in", in), ("load", load), ("out", out), ("clk", clk))

    run(1)

    in <~ true
    run(1)

    in <~ false
    run(1)

    load <~ true
    in <~ true
    run(1)

    load <~ false
    in <~ false
    run(5)

    load <~ true
    run(5)
  }
}

object FlipFlopTest extends App {
  new CircuitSimulation with SequentialElements {
    override val FlipFlopDelay: Int = 1

    val set, reset = new Wire
    val (out, cout) = flipflop(set, reset)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("set", set), ("reset", reset), ("out", out), ("cout", cout))

    run(3)

    set <~ true
    run(3)

    set <~ false
    run(3)

    reset <~ true
    run(3)

    reset <~ false
    run(3)
  }
}

object SumTest extends App {
  new CircuitSimulation with ArithmeticElements {
    val input1, input2, cin = new Wire
    val (sum, carry) = fullAdder(input1, input2, cin)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("a", input1), ("b", input2), ("cin", cin), ("sum", sum), ("carry", carry))

    run(2)

    input1 <~ true
    run(2)

    input2 <~ true
    run(2)

    cin <~ true
    run(2)

    tracer.close()
  }
}

object ClockTest extends App {
  new CircuitSimulation with LogicElements with SequentialElements {
    val clk  = clock(1)
    val clk2 = clock(2)
    val clk3 = clock(3)

    val comb = and(clk, clk2)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("clk1", clk), ("clk2", clk2), ("clk3", clk3), ("and", comb))

    run(20)
  }
}

object MuxTest extends App {
  class MuxTestCircuit extends CircuitSimulation with SimulationStatistics with ControlFlowElements  {
    val a, b, s = new Wire
    val out = mux(a, b, s)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("a", a), ("b", b), ("s", s), ("out", out))

    run(10)

    a <~ true
    run(10)

    s <~ true
    run(10)
  }

  val normal = new MuxTestCircuit
  val opt    = new MuxTestCircuit with OptimizedElements

  assert(normal.actionCount > opt.actionCount)
}

object DeMuxTest extends App {
  new CircuitSimulation with ControlFlowElements with OptimizedElements {
    val a, s = new Wire
    val (outA, outB) = demux(a, s)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("a", a), ("s", s), ("outA", outA), ("outB", outB))

    run(10)

    a <~ true
    run(10)

    s <~ true
    run(10)
  }
}

object BusTest extends App {
  new CircuitSimulation with LogicElements {
    val in, out = new Bus(4)

    inverter(in) ~> out

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("a", in(3)),  ("b", in(2)),  ("c", in(1)),  ("d", in(0)),
                     ("x", out(3)), ("y", out(2)), ("z", out(1)), ("k", out(0)),
                     ("busin", in), ("busout", out))

    run(10)

    in(3) <~ true
    run(10)

    in setSignal 0xF
    run(10)
  }
}

object EightBitAdder extends App {
  new CircuitSimulation with LogicElements with ArithmeticElements with SequentialElements {
    val busA, busB = new Bus(8)
    val (busOut, overflow) = multiBitAdder(busA, busB)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("bus a (in)", busA), ("bus b (in)", busB), ("sum (output)", busOut), ("of", overflow))

    run(1)

    busA setSignal 0xFA

    (1 to 10).foreach { i =>
      busB setSignal i
      run(1)
    }
  }
}

object EightBitIncrementer extends App {
  new CircuitSimulation with LogicElements with ArithmeticElements with SequentialElements {
    val busA = new Bus(8)
    val (busOut, overflow) = multiBitIncrementer(busA)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("bus a (in)", busA), ("sum (output)", busOut), ("of", overflow))

    run(1)

    (0xFC to 0xFF).foreach { i =>
      busA setSignal i
      run(1)
    }
  }
}

object EightBitMultiplexer extends App {
  new CircuitSimulation with LogicElements with ControlFlowElements {
    val busA, busB = new Bus(8)
    val selector = new Wire
    val busOut = mux(busA, busB, selector)

    implicit val tracer = new ConsoleTracer
    tracer.setProbes(("bus a (in)", busA), ("bus b (in)", busB), ("sel", selector), ("sum (output)", busOut))

    run(1)

    busA setSignal 0xFA
    run(1)

    selector <~ true
    run(1)

    busB setSignal 0xFF
    run(1)
  }
}