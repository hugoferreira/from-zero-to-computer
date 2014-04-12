package eu.shiftforward

import java.io.File

object FlipFlopTest extends App {
  new CircuitSimulation with SequentialElements {
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

object MainCircuits extends App {
  val file = new File("/users/bytter/desktop/output2.vcd")

  new CircuitSimulation with ArithmeticElements with SequentialElements {
    override val GenericGateDelay: Int = 0
    override val InverterDelay: Int = 0
    override val FlipFlopDelay: Int = 1

    val input1, input2, cin, sum, carry, clk, clk3 = new Wire
    implicit val probes = List(("a", input1), ("b", input2), ("cin", cin), ("sum", sum), ("carry", carry), ("clock", clk), ("clock3", clk3))
    implicit val tracer = new ConsoleTracer
    // implicit val tracer = new VCDTracer(file)

    tracer.setHeader(probes.map(_._1))

    clock(clk)
    clock(clk3, 3)

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
