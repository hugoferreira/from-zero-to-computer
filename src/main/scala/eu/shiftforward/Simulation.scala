package eu.shiftforward

import scala.collection.SortedMap
import scala.collection.immutable.Queue

abstract class Simulation {
  type Action = () => Unit
  type Agenda = SortedMap[Int, Queue[Action]]

  protected var curtime = 0
  def currentTime: Int = curtime

  private var agenda: Agenda = SortedMap()

  def schedule(delay: Int = 0)(block: => Unit) {
    val time = currentTime + delay
    agenda += (time -> agenda.getOrElse(time, Queue()).enqueue(() => block))
  }

  protected def step() {
    if (!agenda.isEmpty) {
      curtime = agenda.head._1

      while(agenda.contains(curtime)) {
        val actions = agenda(curtime)
        agenda -= curtime
        processActions(actions)
      }
    }
  }

  protected def processActions(actions: Seq[Action]) { actions.foreach(_()) }

  protected def hasNext = !agenda.isEmpty

  def run() {
    schedule() {
      println("*** simulation started, time = " + currentTime + " ***")
    }

    while (!agenda.isEmpty) step()
  }
}

trait SimulationStatistics extends Simulation {
  private var numActions: Int = 0

  override def processActions(actions: Seq[Action]) {
    numActions += actions.size
    super.processActions(actions)
  }

  def actionCount = numActions
}