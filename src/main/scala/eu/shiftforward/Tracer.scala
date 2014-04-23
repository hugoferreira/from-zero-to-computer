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
      case (h: Boolean, s: Boolean)  => prettyPrintSignal(h, s)
      case (_, b: Iterable[Boolean]) => b.map(s => if (s) 1 else 0).mkString.reverse
    } else signals.map {
      case s: Boolean => prettyPrintSignal(s, s)
      case b: Iterable[Boolean] => b.map(s => if (s) 1 else 0).mkString.reverse
    }

    println(currentTime + "\t" + values.mkString("\t"))
    lastValues = signals
  }
}

class VCDTracer(file: java.io.File, secondaryTracer: Tracer = new ConsoleTracer) extends Tracer {
  var probes: List[Probe] = List()

  val pw = new java.io.PrintWriter(file)
  val date = new java.util.Date().toString
  pw.println(s"""$$date
                |  $date
                |$$end
                |$$timescale
                |  1ms
                |$$end""".stripMargin)

  val symbolList = (33 to 126).map(_.asInstanceOf[Char].toString).toList

  def setProbes(ps: Probe*) {
    this.probes = ps.toList
    probes.zip(symbolList).map {
      case ((id, conn: Bus),  symbol) => s"$$var reg ${conn.size} $symbol $id [${conn.size - 1}:0] $$end"
      case ((id,    _: Wire), symbol) => s"$$var reg 1 $symbol $id $$end"
    } foreach pw.println
    pw.println("$enddefinitions $end")

    secondaryTracer.setProbes(ps: _*)
  }

  def currentValues = probes.map(_._2)

  def trace(currentTime: Int) {
    pw.println("#" + currentTime)
    currentValues.zip(symbolList).map {
      case (v: Wire, s) => (if (v.getSignal) 1 else 0) + s
      case (v: Bus, s)  => s"b$v $s"
    } foreach pw.println

    secondaryTracer.trace(currentTime)
  }

  override def close() {
    pw.flush()
    pw.close()

    secondaryTracer.close()
  }
}