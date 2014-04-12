package eu.shiftforward

trait Tracer {
  def setHeader(probes: List[String])
  def trace(currentTime: Int, currentValues: List[Any])
  def close() { }
}

object DummyTracer extends Tracer {
  def setHeader(probes: List[String]) { }
  def trace(currentTime: Int, currentValues: List[Any]) { }
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

  def trace(currentTime: Int, currentValues: List[Any]) {
    val signals = if (!lastValues.isEmpty) lastValues.zip(currentValues).map {
      case (h: Boolean, s: Boolean) => prettyPrintSignal(h, s)
      case (_, s: Any) => s.toString
    } else currentValues.map {
      case s: Boolean => prettyPrintSignal(s, s)
      case s: Any => s.toString
    }

    println(currentTime + "\t" + signals.mkString("\t"))
    lastValues = currentValues
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

  def trace(currentTime: Int, currentValues: List[Any]) {
    pw.println("#" + currentTime)
    currentValues.zip(symbolList).map {
      case (v: Boolean, s) => (if (v) 1 else 0) + s
      case (v: Any, s) => v.toString + s
    } foreach pw.println
  }

  override def close() {
    pw.close()
  }
}