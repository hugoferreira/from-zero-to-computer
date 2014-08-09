package eu.shiftforward.J1

object j1cpuTest extends App {
  val program = List[Microcode](
    Ldc(3),
    Ldc(2),
    Alu(Snd, pushD = true),           // Swap
    Alu(Add, pushD = true, dsd = -1), // Add and store
    Alu(Fst, pushD = true),           // Duplicate
    Alu(Eq)                           // Compare
  )

  val cpu = new j1cpu(program)
  (1 to 100) foreach { _ ⇒ cpu.step() }
}