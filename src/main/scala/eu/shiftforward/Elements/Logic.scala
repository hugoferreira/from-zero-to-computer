package eu.shiftforward.Elements

import eu.shiftforward.{Ground, Bus, Wire, CircuitSimulation}

trait Logic extends CircuitSimulation {
  private def unaryLogicGate(input: Wire)(op: Boolean => Boolean) = {
    val output = new Wire
    input addAction { () =>
      val inputSig = input.getSignal
      schedule(InverterDelay) { output <~ op(inputSig) }
    }
    output
  }

  def inverter(input: Wire): Wire = unaryLogicGate(input) { !_ }
  def inverter(input: Bus): Bus = input map inverter

  def buffer(input: Wire): Wire = unaryLogicGate(input) { identity }
  def buffer(input: Bus): Bus = input map buffer

  private def binaryLogicGate(a: Wire, b: Wire)(op: (Boolean, Boolean) => Boolean) = {
    val output = new Wire
    def action() {
      val inputA = a.getSignal
      val inputB = b.getSignal
      schedule(GenericGateDelay) { output <~ op(inputA, inputB) }
    }

    a addAction action
    b addAction action
    output
  }

  def and(ins: Iterable[Wire]): Wire  = ins reduceLeft and
  def or(ins: Iterable[Wire]): Wire   = ins reduceLeft or
  def xor(ins: Iterable[Wire]): Wire  = ins reduceLeft xor
  def nand(ins: Iterable[Wire]): Wire = ins reduceLeft nand
  def nor(ins: Iterable[Wire]): Wire  = ins reduceLeft nor

  def and(a: Wire, b: Wire)  = binaryLogicGate(a, b) { _ && _ }
  def or(a: Wire, b: Wire)   = binaryLogicGate(a, b) { _ || _ }
  def xor(a: Wire, b: Wire)  = binaryLogicGate(a, b) { _ ^ _ }
  def nand(a: Wire, b: Wire) = binaryLogicGate(a, b) { (x, y) => !(x && y) }
  def nor(a: Wire, b: Wire)  = binaryLogicGate(a, b) { (x, y) => !(x || y) }

  def and(x: Bus, y: Bus): Bus  = (x, y).zipped map and
  def or(x: Bus, y: Bus): Bus   = (x, y).zipped map or
  def xor(x: Bus, y: Bus): Bus  = (x, y).zipped map xor
  def nand(x: Bus, y: Bus): Bus = (x, y).zipped map nand
  def nor(x: Bus, y: Bus): Bus  = (x, y).zipped map nor

  def rotateRight(a: Bus): Bus = buffer(a.drop(1) ++ a.take(1))
  def rotateLeft(a: Bus): Bus  = buffer(a.takeRight(1) ++ a.dropRight(1))
  def shiftRight(a: Bus): Bus  = buffer(a.drop(1) :+ Ground)
  def shiftLeft(a: Bus): Bus   = buffer(Ground +: a.dropRight(1))
}