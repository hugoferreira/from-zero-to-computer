package eu.shiftforward

trait Tracer {
  type Probe = (String, Connector[_])

  def setProbes(probes: Probe*)
  def trace(currentTime: Int)
  def close() { }
}

object DummyTracer extends Tracer {
  def setProbes(probes: Probe*) { }
  def trace(currentTime: Int) { }
}

class ConsoleTracer extends Tracer {
  var lastValues = List[Any]()
  var probes: List[Probe] = List()

  private def prettyPrintSignal(h: Boolean, s: Boolean) = (h, s) match {
    case (false, false) => "│  "
    case (false, true)  => "└─┐"
    case (true, true)   => "  │"
    case (true, false)  => "┌─┘"
  }

  def setProbes(probes: Probe*) {
    this.probes = probes.toList
    println("time\t" + probes.map(_._1).mkString("\t"))
  }

  def currentValues = probes.map(_._2)

  def trace(currentTime: Int) {
    val signals = currentValues.map(_.getSignal)
    val values  = if (!lastValues.isEmpty) lastValues.zip(signals).map {
      case (h: Boolean, s: Boolean) => prettyPrintSignal(h, s)
      case (_, b: Iterable[Boolean]) => b.map(s => if (s) 1 else 0).mkString.reverse
    } else signals.map {
      case s: Boolean => prettyPrintSignal(s, s)
      case b: Iterable[Boolean] => b.map(s => if (s) 1 else 0).mkString.reverse
    }

    println(currentTime + "\t" + values.mkString("\t"))
    lastValues = signals
  }
}

class VCDTracer(file: java.io.File) extends Tracer {
  var probes: List[Probe] = List()

  val pw = new java.io.PrintWriter(file)
  pw.println("$date\n  " + new java.util.Date().toString + "\n$end\n$timescale\n	1ms\n$end")

  val symbolList = (33 to 126).map(_.asInstanceOf[Char].toString).toList

  def setProbes(probes: Probe*) {
    this.probes = probes.toList
    probes.map(_._1).zip(symbolList).map { case (probe, symbol) => "$var reg 1 " + symbol + " " + probe + " $end" } foreach pw.println
    pw.println("$enddefinitions $end")
  }

  def currentValues = probes.map(_._2)

  def trace(currentTime: Int) {
    pw.println("#" + currentTime)
    currentValues.zip(symbolList).map {
      case (v: Wire, s) => (if (v.getSignal) 1 else 0) + s
      case (v: Bus, s) => v.toString + s
    } foreach pw.println
  }

  override def close() {
    pw.close()
  }
}