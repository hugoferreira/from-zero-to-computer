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
}

object MainCircuits extends App {
  import eu.shiftforward.CircuitSimulation._

  val input1, input2, sum, carry = new Wire
  val probes = ("a", input1) :: ("b", input2) :: ("sum", sum) :: ("carry", carry) :: Nil

  halfAdder(input1, input2, sum, carry)

  input1 setSignal true
  run(probes)

  input2 setSignal true
  run(probes, printheader = false)
}
