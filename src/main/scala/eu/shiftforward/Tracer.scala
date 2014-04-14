package eu.shiftforward

trait Tracer {
  def setHeader(probes: List[String])
  def trace(currentTime: Int, currentValues: List[Connector[_]])
  def close() { }
}

object DummyTracer extends Tracer {
  def setHeader(probes: List[String]) { }
  def trace(currentTime: Int, currentValues: List[Connector[_]]) { }
}

class ConsoleTracer extends Tracer {
  var lastValues = List[Any]()

  private def prettyPrintSignal(h: Boolean, s: Boolean) = (h, s) match {
    case (false, false) => "│  "
    case (false, true)  => "└─┐"
    case (true, true)   => "  │"
    case (true, false)  => "┌─┘"
  }

  def setHeader(probes: List[String]) {
    println("time\t" + probes.mkString("\t"))
  }

  def trace(currentTime: Int, currentValues: List[Connector[_]]) {
    val signals = currentValues.map(_.getSignal)
    val values  = if (!lastValues.isEmpty) lastValues.zip(signals).map {
      case (h: Boolean, s: Boolean) => prettyPrintSignal(h, s)
      case (_, b: Iterable[Boolean]) => b.map(s => if (s) 1 else 0).mkString
    } else signals.map {
      case s: Boolean => prettyPrintSignal(s, s)
      case b: Iterable[Boolean] => b.map(s => if (s) 1 else 0).mkString
    }

    println(currentTime + "\t" + values.mkString("\t"))
    lastValues = signals
  }
}

class VCDTracer(file: java.io.File) extends Tracer {
  val pw = new java.io.PrintWriter(file)
  pw.println("$date\n  " + new java.util.Date().toString + "\n$end\n$timescale\n	1ms\n$end")

  val symbolList = (33 to 126).map(_.asInstanceOf[Char].toString).toList

  def setHeader(probes: List[String]) {
    probes.zip(symbolList).map { case (probe, symbol) => "$var reg 1 " + symbol + " " + probe + " $end" } foreach pw.println
    pw.println("$enddefinitions $end")
  }

  def trace(currentTime: Int, currentValues: List[Connector[_]]) {
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