package eu.shiftforward.Elements

import eu.shiftforward.{Wire, CircuitSimulation}

trait Sequential extends CircuitSimulation {
  def clock(interval: Int = 1, signal: Boolean = false) = {
    val out = new Wire
    out <~ signal
    ticktack(out, interval, signal)
    out
  }

  private def ticktack(out: Wire, interval: Int = 1, signal: Boolean = false) {
    schedule(interval) {
      out <~ signal
      ticktack(out, interval, !signal)
    }
  }

  def dff(in: Wire, initState: Boolean = false)(implicit clock: Wire) = {
    var state = initState
    val out = new Wire

    def action() {
      val clockIn = clock.getSignal
      val inputA  = in.getSignal
      schedule() {
        if (clockIn) {
          state = inputA
          out <~ state
        }
      }
    }

    clock addAction action

    out
  }

  def flipflop(set: Wire, reset: Wire, initState: Boolean = false) = {
    val out, cout = new Wire
    var state = initState

    def action() {
      val isSet = set.getSignal
      val isReset = reset.getSignal

      schedule(FlipFlopDelay) {
        state = isSet || (state && !isReset)

        out  <~ state
        cout <~ !state
      }
    }

    set addAction action
    reset addAction action

    (out, cout)
  }
}