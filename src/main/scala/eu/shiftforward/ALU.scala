package eu.shiftforward

class ALU extends ArithmeticElements with ControlFlowElements {
  def alu(x: Bus, y: Bus, zx: Wire, nx: Wire, zy: Wire, ny: Wire, f: Wire, no: Wire): (Bus, Wire, Wire, Wire) = {
    assert(x.size == y.size)

    val zeros = constant(x.size)(0x00)

    val x1 = mux(x, zeros, zx)
    val y1 = mux(y, zeros, zy)

    val x2 = mux(x1, inverter(x1), nx)
    val y2 = mux(y1, inverter(y1), ny)

    val (sum, of) = multiBitAdder(x2, y2)
    val out = mux(and(x2, y2), sum, f)

    (mux(out, inverter(out), no), Ground, Ground, mux(Ground, of, f))
  }
}
