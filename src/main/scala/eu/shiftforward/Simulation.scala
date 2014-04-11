package eu.shiftforward

import scala.collection.SortedMap

abstract class Simulation {
  type Action = () => Unit
  type Agenda = SortedMap[Int, List[WorkItem]]

  case class WorkItem(time: Int, action: Action)

  private var curtime = 0
  def currentTime: Int = curtime

  private var agenda: Agenda = SortedMap()

  def schedule(delay: Int = 0)(block: => Unit) {
    val item = WorkItem(currentTime + delay, () => block)
    agenda += (item.time -> (item :: agenda.getOrElse(item.time, List())))
  }

  protected def next() {
    if (!agenda.isEmpty) {
      val item = agenda.head
      curtime  = item._1
      agenda  -= curtime
      item._2.foreach(_.action())
    }
  }

  protected def hasNext = !agenda.isEmpty

  def run() {
    schedule() {
      println("*** simulation started, time = " + currentTime + " ***")
    }

    while (!agenda.isEmpty) next()
  }
}