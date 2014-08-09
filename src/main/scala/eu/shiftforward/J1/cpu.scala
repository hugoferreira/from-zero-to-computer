package eu.shiftforward.J1

class j1cpu(program: Seq[Short], ramSize: Int = 65536, stackSize: Int = 32) {
  def this(program: Seq[Microcode]) = this(program.map(_.encode))

  // Memory Structures
  val ram = program.toArray ++ Array.fill(ramSize - program.length)(0.toShort)
  val dstack = Array.fill(stackSize)(0.toShort)
  val rstack = Array.fill(stackSize)(0.toShort)

  // Registers
  var st0  = 0.toShort
  def st1  = dstack(dsp)
  def rst0 = rstack(rsp)

  // Pointers
  var pc, rsp, dsp = 0.toShort

  def step() = {
    val instruction = Microcode(ram(pc))

    val _st0 = (instruction match {
      case Ldc(v) ⇒ v & 0x7FFF
      case Alu(Fst, _, _, _, _, _, _) ⇒ st0
      case Alu(Snd, _, _, _, _, _, _) ⇒ st1
      case Alu(Add, _, _, _, _, _, _) ⇒ st0 + st1
      case Alu(And, _, _, _, _, _, _) ⇒ st0 & st1
      case Alu(Or,  _, _, _, _, _, _) ⇒ st0 | st1
      case Alu(Xor, _, _, _, _, _, _) ⇒ st0 ^ st1
      case Alu(Not, _, _, _, _, _, _) ⇒ st0 ^ 0xFFFF
      case Alu(Eq,  _, _, _, _, _, _) ⇒ if (st1 == st0) 1 else 0
      case Alu(Lt,  _, _, _, _, _, _) ⇒ if (st1  < st0) 1 else 0
      case Alu(ULt, _, _, _, _, _, _) ⇒ if (st1  < st0) 1 else 0 // FIXME: unsigned comparision
      case Alu(Shr, _, _, _, _, _, _) ⇒ st1 >> st0
      case Alu(Shl, _, _, _, _, _, _) ⇒ st1 << st0
      case Alu(Dec, _, _, _, _, _, _) ⇒ st0 - 1
      case Alu(Ret, _, _, _, _, _, _) ⇒ rst0
      case Alu(Ld,  _, _, _, _, _, _) ⇒ ram(st0)
      case Alu(Dph, _, _, _, _, _, _) ⇒ dstack.length            // FIXME: + 000 + rstack.length
      case _ ⇒ st0
    }).toShort

    instruction match {
      case Ldc(_) ⇒
        dsp = (dsp + 1).toShort
        dstack(dsp) = st0
        pc = (pc + 1).toShort
      case Jmp(a) ⇒
        pc = a
      case Jmz(a) ⇒
        dsp = (dsp - 1).toShort
        pc = if (st0 == 0) a else (pc + 1).toShort
      case Call(a) ⇒
        rstack(rsp) = (pc + 1).toShort
        rsp = (rsp + 1).toShort
        pc = a
      case Alu(_, ret, push, pushAddr, store, rsd, dsd) ⇒
        val r = rstack(dsp)

        if (pushAddr) { rstack(rsp) = st0 }
        if (push)     { dstack(dsp) = st0 }
        if (store)    { ram(st0)    = st1 }

        dsp = (dsp + dsd).toShort
        rsp = (rsp + rsd).toShort

        pc = if (ret) r else (pc + 1).toShort
    }

    st0 = _st0

    println(s"$pc: $instruction\t(${instruction.encode.toBinaryString.reverse.padTo(16, '0').take(16).reverse}, rsp $rsp, dsp $dsp, st0 $st0, st1 $st1)")
  }
}
