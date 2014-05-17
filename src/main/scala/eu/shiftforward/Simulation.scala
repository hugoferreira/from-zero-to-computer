package eu.shiftforward

import scala.collection.SortedMap
import scala.collection.immutable.Queue

abstract class Simulation {
  type Action = () => Unit
  type Agenda = SortedMap[Long, Queue[Action]]
  type Trigger = (() => Boolean, () => Unit)

  protected var curtime = 0l
  def currentTime: Long = curtime

  private var agenda: Agenda = SortedMap()
  private var triggers: List[Trigger] = List()

  def schedule(delay: Int = 0)(block: => Unit) {
    val time = currentTime + delay
    agenda += (time -> agenda.getOrElse(time, Queue()).enqueue(() => block))
  }

  def schedule(condition: => Boolean)(action: => Unit) {
    triggers = (() => condition, () => action) :: triggers
  }

  protected def step() {
    if (hasNext) {
      curtime = agenda.head._1

      while(agenda.contains(curtime)) {
        val actions = agenda(curtime)
        agenda -= curtime
        processActions(actions)
        processTriggers()
      }
    }
  }

  protected def processActions(actions: Seq[Action]) { actions.foreach(_()) }
  protected def processTriggers() { triggers.filter(_._1()).foreach(_._2()) }

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