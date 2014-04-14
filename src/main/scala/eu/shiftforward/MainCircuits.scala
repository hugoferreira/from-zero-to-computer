package eu.shiftforward

import java.io.File

object FlipFlopTest extends App {
  new CircuitSimulation with SequentialElements {
    override val FlipFlopDelay: Int = 1

    val set, reset, out, cout = new Wire
    implicit val probes = List(("set", set), ("reset", reset), ("out", out), ("cout", cout))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    flipflop(set, reset, out, cout)
    run(3)

    set setSignal true
    run(3)

    set setSignal false
    run(3)

    reset setSignal true
    run(3)

    reset setSignal false
    run(3)
  }
}

object SumTest extends App {
  new CircuitSimulation with ArithmeticElements {
    val input1, input2, cin, sum, carry = new Wire
    implicit val probes = List(("a", input1), ("b", input2), ("cin", cin), ("sum", sum), ("carry", carry))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    fullAdder(input1, input2, cin, sum, carry)
    run(2)

    input1 setSignal true
    run(2)

    input2 setSignal true
    run(2)

    cin setSignal true
    run(2)

    tracer.close()
  }
}

object ClockTest extends App {
  new CircuitSimulation with LogicElements with SequentialElements {
    val clk, clk2, comb = new Wire

    implicit val probes = List(("clk1", clk), ("clk2", clk2), ("and", comb))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    clock(clk)
    clock(clk2, 2)

    and(clk, clk2, comb)

    run(10)
  }
}

object MuxTest extends App {
  class MuxTestCircuit extends CircuitSimulation with SimulationStatistics with ControlFlowElements  {
    val a, b, s, out = new Wire
    implicit val probes = List(("a", a), ("b", b), ("s", s), ("out", out))

    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    mux(a, b, s, out)
    run(10)

    a setSignal true
    run(10)

    s setSignal true
    run(10)
  }

  val normal = new MuxTestCircuit
  val opt    = new MuxTestCircuit with OptimizedElements

  assert(normal.actionCount > opt.actionCount)
}

object DeMuxTest extends App {
  new CircuitSimulation with ControlFlowElements with OptimizedElements {
    val a, s, outA, outB = new Wire

    implicit val probes = List(("a", a), ("s", s), ("outA", outA), ("outB", outB))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    demux(a, s, outA, outB)
    run(10)

    a setSignal true
    run(10)

    s setSignal true
    run(10)
  }
}

object BusTest extends App {
  new CircuitSimulation with LogicElements {
    val a, b, c, d = new Wire
    val x, y, z, k = new Wire
    val busIn  = new Bus(a, b, c, d)
    val busOut = new Bus(x, y, z, k)

    implicit val probes = List(("a", a), ("b", b), ("c", c), ("d", d), ("x", x), ("y", y), ("z", z), ("k", k), ("busin", busIn), ("busout", busOut))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    busIn zip busOut foreach { case (i, o) => inverter(i, o) }

    run(10)

    a setSignal true
    run(10)

    busIn setSignal 0xF
    run(10)
  }
}