package eu.shiftforward.Elements

import eu.shiftforward.{Bus, Wire}

trait ControlFlow extends Logic {
  def mux(a: Wire, b: Wire, s: Wire): Wire =
    or(and(a, inverter(s)), and(b, s))

  def mux(a: Bus, b: Bus, selector: Wire): Bus =
    (a, b).zipped map { mux(_, _, selector) }

  def mux(a: Iterable[Bus], selector: Bus): Bus = selector.reverse match {
    case h +: Seq() =>
      mux(a.drop(1).head, a.head, h)
    case h +: t =>
      mux(mux(a.drop(a.size / 2), t.reverse), mux(a.take(a.size / 2), t.reverse), h)
  }

  def demux(a: Wire, s: Wire): (Wire, Wire) =
    (and(a, inverter(s)), and(a, s))

  def demux(a: Bus, s: Wire): (Bus, Bus) = {
    val (x, y) = (a map { x => demux(x, s) }).unzip
    (x, y)
  }
}