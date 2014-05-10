package eu.shiftforward.Elements

import eu.shiftforward.{Ground, Bus, Wire}

trait Arithmetic extends Logic {
  def halfAdder(a: Wire, b: Wire) = {
    val c = and(a, b)
    (and(or(a, b), inverter(c)), c)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire) = {
    val (s,   c1) = halfAdder(a, cin)
    val (sum, c2) = halfAdder(b, s)
    (sum, or(c1, c2))
  }

  def multiBitAdder(a: Bus, b: Bus): (Bus, Wire) = {
    val (wires, overflow) = (a zip b).foldLeft((List[Wire](), Ground: Wire)) {
      case ((bus, carry), (a, b)) =>
        val (c1, cout) = fullAdder(a, b, carry)
        (bus :+ c1, cout)
    }

    (wires, overflow)
  }

  def multiBitIncrementer(a: Bus) =
    multiBitAdder(a, constant(a.size)(0x01))

  lazy val constant = (width: Int) => (c: Int) => {
    val bus = new Bus(width)
    bus.setSignal(c)
    bus
  }

  def cmp0(a: Bus): Wire = inverter(or(a))
  def cmp(a: Bus, b: Bus): Wire = inverter(or(xor(a, b)))
}

trait SequentialArithmetic extends Arithmetic with Memory {
  def counter(width: Int)(reset: Wire)(implicit clock: Wire) = {
    val in = new Bus(width)
    val reg = register(in, true)
    val (inc, _) = multiBitIncrementer(reg)
    mux(inc, constant(width)(0), reset) ~> in
    reg
  }
}