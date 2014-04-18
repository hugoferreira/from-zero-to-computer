package eu.shiftforward

import java.io.File

object FlipFlopTest extends App {
  new CircuitSimulation with SequentialElements {
    override val FlipFlopDelay: Int = 1

    val set, reset = new Wire
    val (out, cout) = flipflop(set, reset)

    implicit val probes = List(("set", set), ("reset", reset), ("out", out), ("cout", cout))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    run(3)

    set ~> true
    run(3)

    set ~> false
    run(3)

    reset ~> true
    run(3)

    reset ~> false
    run(3)
  }
}

object SumTest extends App {
  new CircuitSimulation with ArithmeticElements {
    val input1, input2, cin = new Wire
    val (sum, carry) = fullAdder(input1, input2, cin)

    implicit val probes = List(("a", input1), ("b", input2), ("cin", cin), ("sum", sum), ("carry", carry))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    run(2)

    input1 ~> true
    run(2)

    input2 ~> true
    run(2)

    cin ~> true
    run(2)

    tracer.close()
  }
}

object ClockTest extends App {
  new CircuitSimulation with LogicElements with SequentialElements {
    val clk, clk2 = new Wire

    clock(clk)
    clock(clk2, 2)

    val comb = and(clk, clk2)

    implicit val probes = List(("clk1", clk), ("clk2", clk2), ("and", comb))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    run(10)
  }
}

object MuxTest extends App {
  class MuxTestCircuit extends CircuitSimulation with SimulationStatistics with ControlFlowElements  {
    val a, b, s = new Wire
    val out = mux(a, b, s)

    implicit val probes = List(("a", a), ("b", b), ("s", s), ("out", out))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    run(10)

    a ~> true
    run(10)

    s ~> true
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

    implicit val probes = List(("a", a), ("s", s), ("outA", outA), ("outB", outB))
    implicit val tracer = new ConsoleTracer

    tracer.setHeader(probes.map(_._1))

    run(10)

    a ~> true
    run(10)

    s ~> true
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

    busIn zip busOut foreach { case (i, o) => connect(inverter(i), o) }

    run(10)

    a ~> true
    run(10)

    busIn setSignal 0xF
    run(10)
  }
}

object EightBitAdder extends App {
  new CircuitSimulation with LogicElements with ArithmeticElements with SequentialElements {
    val busA, busB = new Bus(8)
    val (busOut, overflow) = multiBitAdder(busA, busB)

    implicit val probes = List(("bus a (in)", busA), ("bus b (in)", busB), ("sum (output)", busOut), ("of", overflow))
    implicit val tracer = new ConsoleTracer
    tracer.setHeader(probes.map(_._1))

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

    implicit val probes = List(("bus a (in)", busA), ("sum (output)", busOut), ("of", overflow))
    implicit val tracer = new ConsoleTracer
    tracer.setHeader(probes.map(_._1))

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
    val busOut = multiBitMultiplexer(busA, busB, selector)

    implicit val probes = List(("bus a (in)", busA), ("bus b (in)", busB), ("sel", selector), ("sum (output)", busOut))
    implicit val tracer = new ConsoleTracer
    tracer.setHeader(probes.map(_._1))

    run(1)

    busA setSignal 0xFA
    run(1)

    selector ~> true
    run(1)

    busB setSignal 0xFF
    run(1)
  }
}