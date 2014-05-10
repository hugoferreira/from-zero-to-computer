package eu.shiftforward

import java.io.File
import eu.shiftforward.Elements._

trait SimulationApp extends App {
  implicit val tracer = new VCDTracer(new File("/tmp/output.vcd"))

  def shutdown() = tracer.close()
}

object DffTest extends SimulationApp {
  new CircuitSimulation with Sequential {
    implicit val clk = clock(1)
    val in = new Wire
    val out = dff(in)

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

  shutdown()
}

object OneBitRegisterTest extends SimulationApp {
  new CircuitSimulation with Memory {
    implicit val clk = clock(1)
    val in, load = new Wire
    val out = register(in, load)

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

  shutdown()
}

object RegisterTest extends SimulationApp {
  new CircuitSimulation with Memory {
    implicit val clk = clock(1)
    val load = new Wire
    val in   = new Bus(8)
    val out  = register(in, load)

    tracer.setProbes(("in", in), ("load", load), ("out", out), ("clk", clk))

    run(1)

    in   <~ 0x0F
    run(1)

    load <~ true
    run(2)

    load <~ false
    in   <~ 0x00
    run(5)

    load <~ true
    in   <~ 0xF0
    run(5)
  }

  shutdown()
}

object RamTest extends SimulationApp {
  new CircuitSimulation with Memory {
    implicit val clk = clock(1)

    val addr, load = new Wire
    val data = new Bus(8)
    val out  = ram(data, addr, load)

    tracer.setProbes(("data\t", data), ("addr", addr), ("load", load), ("out\t", out), ("clk", clk))

    run(1)

    addr <~ true
    data <~ 0x0F
    run(5)

    load <~ true
    run(5)

    load <~ false
    addr <~ false
    run(5)
  }

  shutdown()
}

object MultipleMuxTest extends SimulationApp {
  new CircuitSimulation with Memory {
    val sel = new Bus(3)
    val a, b, c, d, e, f, g, h = new Bus(4)
    val out = mux(List(a, b, c, d, e, f, g, h), sel)

    tracer.setProbes(("a", a), ("b", b), ("c", c), ("d", d), ("e", e), ("f", f), ("g", g), ("h", h), ("sel", sel), ("out", out))

    run(1)

    a <~ 0x01
    b <~ 0x02
    c <~ 0x03
    d <~ 0x04
    e <~ 0x05
    f <~ 0x06
    g <~ 0x07
    h <~ 0x08

    (0 to 7).foreach { s =>
      sel <~ s
      run(1)
    }
  }

  shutdown()
}

object RegisterBasedALUTest extends SimulationApp {
  new CircuitSimulation with Arithmetic with Memory {
    implicit val clk = clock(1)
    val loadA, loadB, loadSum = new Wire
    val a, b = new Bus(8)
    val r1 = register(a, loadA)
    val r2 = register(b, loadB)
    val (sum, _) = multiBitAdder(r1, r2)
    val bta = and(r1, r2)
    val r3 = register(sum, loadSum)
    val r4 = register(bta, loadSum)

    tracer.setProbes(("a", a), ("r1", r1), ("b", b), ("r2", r2), ("r3", sum), ("r4", bta), ("clk", clk), ("loadA", loadA), ("loadB", loadB), ("load", loadSum))

    a       <~ 0x01
    b       <~ 0x01
    run(5)

    loadA   <~ true
    loadB   <~ true
    loadSum <~ false
    run(5)

    loadSum <~ true
    run(5)

    tracer.close()
  }

  shutdown()
}

object FlipFlopTest extends SimulationApp {
  new CircuitSimulation with Sequential {
    override val FlipFlopDelay: Int = 1

    val set, reset = new Wire
    val (out, cout) = flipflop(set, reset)

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

  shutdown()
}

object SumTest extends SimulationApp {
  new CircuitSimulation with Arithmetic {
    val input1, input2, cin = new Wire
    val (sum, carry) = fullAdder(input1, input2, cin)

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

  shutdown()
}

object ClockTest extends SimulationApp {
  new CircuitSimulation with Logic with Sequential {
    val clk  = clock(1)
    val clk2 = clock(2)
    val clk3 = clock(3)

    val comb = and(clk, clk2)

    tracer.setProbes(("clk1", clk), ("clk2", clk2), ("clk3", clk3), ("and", comb))

    run(20)
  }

  shutdown()
}


object MuxTest extends SimulationApp {
  class MuxTestCircuit extends CircuitSimulation with SimulationStatistics with ControlFlow {
    val a, b, s = new Wire
    val out = mux(a, b, s)

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

object DeMuxTest extends SimulationApp {
  new CircuitSimulation with ControlFlow with OptimizedElements {
    val a, s = new Wire
    val (outA, outB) = demux(a, s)

    tracer.setProbes(("a", a), ("s", s), ("outA", outA), ("outB", outB))

    run(10)

    a <~ true
    run(10)

    s <~ true
    run(10)
  }

  shutdown()
}

object BusTest extends SimulationApp {
  new CircuitSimulation with Logic {
    val in, out = new Bus(4)

    inverter(in) ~> out

    tracer.setProbes(("a", in(3)),  ("b", in(2)),  ("c", in(1)),  ("d", in(0)),
                     ("x", out(3)), ("y", out(2)), ("z", out(1)), ("k", out(0)),
                     ("busin", in), ("busout", out))

    run(10)

    in(3) <~ true
    run(10)

    in <~ 0xF
    run(10)
  }

  shutdown()
}

object BitShuffleTest extends SimulationApp {
  new CircuitSimulation with Logic {
    val in = new Bus(4)
    val rol = rotateLeft(in)
    val ror = rotateRight(in)
    val shl = shiftLeft(in)
    val shr = shiftRight(in)

    tracer.setProbes(("in", in), ("rotL", rol), ("rotR", ror), ("shiftL", shl), ("shiftR", shr))

    (0 to 10).foreach { i =>
        in <~ i
        run(1)
    }
  }

  shutdown()
}

object CmpTest extends SimulationApp {
  new CircuitSimulation with Arithmetic {
    val in = new Bus(4)
    val cmpZero = cmp0(in)
    val cmpTen  = cmp(in, constant(4)(0x0A))

    tracer.setProbes(("in", in), ("cmp0", cmpZero), ("cmpTen", cmpTen))

    (0 to 10).foreach { i =>
      in <~ i
      run(1)
    }
  }

  shutdown()
}


object EightBitAdder extends SimulationApp {
  new CircuitSimulation with Logic with Arithmetic with Sequential {
    val busA, busB = new Bus(8)
    val (busOut, overflow) = multiBitAdder(busA, busB)

    tracer.setProbes(("bus a (in)", busA), ("bus b (in)", busB), ("sum (output)", busOut), ("of", overflow))

    run(1)

    busA <~ 0xFA

    (1 to 10).foreach { i =>
      busB <~ i
      run(1)
    }
  }

  shutdown()
}

object EightBitIncrementer extends SimulationApp {
  new CircuitSimulation with Logic with Arithmetic with Sequential {
    val busA = new Bus(8)
    val (busOut, overflow) = multiBitIncrementer(busA)

    tracer.setProbes(("bus a (in)", busA), ("sum (output)", busOut), ("of", overflow))

    run(1)

    (0xFC to 0xFF).foreach { i =>
      busA <~ i
      run(1)
    }
  }

  shutdown()
}

object EightBitCounter extends SimulationApp {
  new CircuitSimulation with SequentialArithmetic {
    implicit val clk = clock(1)

    val reset = new Wire
    val pc = counter(8)(reset)

    tracer.setProbes(("reset", reset), ("counter\t", pc), ("clk", clk))

    runUntil { pc is 0x05 }

    reset <~ true
    run(10)

    reset <~ false
    runUntil { pc is 0x05 }
  }

  shutdown()
}

object EightBitMultiplexer extends SimulationApp {
  new CircuitSimulation with Logic with ControlFlow {
    val busA, busB = new Bus(8)
    val selector = new Wire
    val busOut = mux(busA, busB, selector)

    tracer.setProbes(("busA", busA), ("busB", busB), ("sel", selector), ("sum", busOut))

    run(1)

    busA <~ 0xFA
    run(1)

    selector <~ true
    run(1)

    busB <~ 0xFF
    run(1)
  }

  shutdown()
}