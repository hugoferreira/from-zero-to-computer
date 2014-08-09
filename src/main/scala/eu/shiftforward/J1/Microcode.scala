package eu.shiftforward.J1

sealed trait Microcode { def encode: Short }
case class Jmp(a: Short)  extends Microcode { def encode = (0x1FFF & a).toShort }                  // 000x xxxx xxxx xxxx
case class Jmz(a: Short)  extends Microcode { def encode = ((0x1FFF & a) | (1 << 13)).toShort }    // 001x xxxx xxxx xxxx
case class Call(a: Short) extends Microcode { def encode = ((0x1FFF & a) | (1 << 14)).toShort }    // 010x xxxx xxxx xxxx
case class Ldc(v: Short)  extends Microcode {
  def encode = ((0xFFFF & v) | (1 << 15)).toShort    // 1xxx xxxx xxxx xxxx
  override def toString = s"ldc ${v & 0x7FFF}"
}

case class Alu(op: AluOp, ret: Boolean = false, pushD: Boolean = false, pushR: Boolean = false,    // 011x xxxx xxxx xxxx
               store: Boolean = false, rsd: Byte = 0, dsd: Byte = 0) extends Microcode {
  def encode = ((               3 << 13       ) |    // .11. .... .... ....
                (if (ret)       1 << 12 else 0) |    // ...1 .... .... ....
                (if (pushD)     1 << 7  else 0) |    // .... .... 1... ....
                (if (pushR)     1 << 6  else 0) |    // .... .... .1.. ....
                (if (store)     1 << 5  else 0) |    // .... .... ..1. ....
                (if (rsd ==  1) 1 << 2  else 0) |    // .... .... .... .1..
                (if (rsd == -1) 3 << 2  else 0) |    // .... .... .... 11..
                (if (dsd ==  1) 1       else 0) |    // .... .... .... ...1
                (if (dsd == -1) 3       else 0) |    // .... .... .... ..11
                op.encode << 8).toShort              // .... xxxx .... ....

  override def toString = List(op.toString.toLowerCase,
                               if (ret) "R → PC" else "",
                               if (pushD) "T → N" else "",
                               if (pushR) "T → R" else "",
                               if (store) "N → [T]" else "",
                               if (rsd != 0) s"RSP $rsd" else "",
                               if (dsd != 0) s"DSP $dsd" else "").filter(_ != "").mkString(", ")
}

sealed trait AluOp            { def encode: Short }
case object Fst extends AluOp { def encode = 0x0 } // T
case object Snd extends AluOp { def encode = 0x1 } // N
case object Add extends AluOp { def encode = 0x2 } // T + N
case object And extends AluOp { def encode = 0x3 } // T & N
case object Or extends AluOp  { def encode = 0x4 } // T | N
case object Xor extends AluOp { def encode = 0x5 } // T ^ N
case object Not extends AluOp { def encode = 0x6 } // ~T
case object Eq extends AluOp  { def encode = 0x7 } // T == N
case object Lt extends AluOp  { def encode = 0x8 } // N < T   (signed)
case object Shr extends AluOp { def encode = 0x9 } // N >> T
case object Dec extends AluOp { def encode = 0xA } // T--
case object Ret extends AluOp { def encode = 0xB } // R
case object Ld extends AluOp  { def encode = 0xC } // [T]
case object Shl extends AluOp { def encode = 0xD } // N << T
case object Dph extends AluOp { def encode = 0xE } // rsp + 000 + dsp
case object ULt extends AluOp { def encode = 0xF } // N (<) T (unsigned)

case object Microcode {
  implicit def toSeq(s: Short): IndexedSeq[Short] = (15 to 0 by -1).map(v ⇒ ((s >> v) & 0x1).toShort)
  implicit def toShort(s: IndexedSeq[Short]): Short = (0 to 15).map(v ⇒ if (s.contains(v)) s(v) << v else 0).sum.toShort

  def apply(code: Short): Microcode = (code: IndexedSeq[Short]) match {
    case 1 +: v ⇒ Ldc(v)
    case 0 +: 0 +: 0 +: v ⇒ Jmp(v)
    case 0 +: 0 +: 1 +: v ⇒ Jmz(v)
    case 0 +: 1 +: 0 +: v ⇒ Call(v)
    case 0 +: 1 +: 1 +: ret +: alu3 +: alu2 +: alu1 +: alu0 +: pushD +: pushR +: store +: _ +: rsd1 +: rsd0 +: dsd1 +: dsd0 +: _ ⇒
      Alu((alu3, alu2, alu1, alu0) match {
        case (0, 0, 0, 0) ⇒ Fst
        case (0, 0, 0, 1) ⇒ Snd
        case (0, 0, 1, 0) ⇒ Add
        case (0, 0, 1, 1) ⇒ And
        case (0, 1, 0, 0) ⇒ Or
        case (0, 1, 0, 1) ⇒ Xor
        case (0, 1, 1, 0) ⇒ Not
        case (0, 1, 1, 1) ⇒ Eq
        case (1, 0, 0, 0) ⇒ Lt
        case (1, 0, 0, 1) ⇒ Shr
        case (1, 0, 1, 0) ⇒ Dec
        case (1, 0, 1, 1) ⇒ Ret
        case (1, 1, 0, 0) ⇒ Ld
        case (1, 1, 0, 1) ⇒ Shl
        case (1, 1, 1, 0) ⇒ Dph
        case (1, 1, 1, 1) ⇒ ULt
      }, ret == 1, pushD == 1, store == 1, pushR == 1, if (rsd1 == 1 & rsd0 == 1) -1.toByte else rsd0.toByte, if (dsd1 == 1 & dsd0 == 1) -1.toByte else dsd0.toByte)
  }
}