package eu.shiftforward

object CircuitSimulation extends BasicCircuitSimulation {
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

  def clock(out: Wire, interval: Int = 2, signal: Boolean = false) {
    schedule(interval) {
      out setSignal signal
      clock(out, interval, signal = !signal)
    }
  }
}

object MainCircuits extends App {
  import eu.shiftforward.CircuitSimulation._

  implicit val tracer = new ConsoleTracer
  val input1, input2, cin, sum, carry, clk, clk3 = new Wire
  val probes = ("a", input1) :: ("b", input2) :: ("cin", cin) :: ("sum", sum) :: ("carry", carry) :: ("clock", clk) :: ("clock 3", clk3) :: Nil

  tracer.setHeader(probes.map(_._1))

  clock(clk)
  clock(clk3, 3)

  fullAdder(input1, input2, cin, sum, carry)
  run(probes, cycles = 10)

  input1 setSignal true
  run(probes, cycles = 10)

  input2 setSignal true
  run(probes, cycles = 10)
}
