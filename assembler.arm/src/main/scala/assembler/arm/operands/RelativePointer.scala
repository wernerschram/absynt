package assembler.arm.operands

import scala.language.implicitConversions

class RelativePointer private(val displacement: Int) extends Operand {
  assume(((displacement & 0xC0000000) == 0) || ((displacement & 0xC0000000) == 0xC0000000))
  assume((displacement & 0x00000003) == 0)

  def encode = (displacement >> 2) & 0xFFFFFF

  override def toString() = s"${displacement.toString()}"
}

object RelativePointer {
  implicit def apply(displacement: Int) = new RelativePointer(displacement)
}