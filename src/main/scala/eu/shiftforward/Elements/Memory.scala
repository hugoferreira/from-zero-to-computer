package eu.shiftforward.Elements

import eu.shiftforward.{Bus, Wire}

trait Memory extends Sequential with ControlFlow {
  /* def register(in: Wire, load: Wire)(implicit clock: Wire) = {
    val muxOut = new Wire
    val out = dff(muxOut)
    mux(out, in, load) ~> muxOut
    out
  } */

  def register(in: Wire, load: Wire)(implicit clock: Wire) = {
    var state = false
    val out = new Wire

    def action() {
      val clockIn = clock.getSignal
      val loadIn = load.getSignal
      val inputA = in.getSignal
      schedule(ClockedGateDelay) {
        if (clockIn && loadIn) {
          state = inputA
          out <~ state
        }
      }
    }

    clock addAction action

    out
  }

  def register(in: Bus, load: Wire)(implicit clock: Wire): Bus = in map { register(_, load) }

  def ram(data: Bus, address: Wire, load: Wire)(implicit clock: Wire): Bus = {
    val (x, y) = demux(load, address)
    val a = register(data, x)
    val b = register(data, y)
    mux(a, b, address)
  }

  /* def ram(data: Bus, address: Bus, load: Wire)(implicit clock: Wire): Bus = {
    val (x, y) = demux(load, address)
    val a = register(data, x)
    val b = register(data, y)
    mux(a, b, address)
  } */

  def rom(address: Bus, contents: Iterable[Int])(implicit clock: Wire): Bus = ???
}