package assembler.arm.operands

import scala.language.implicitConversions

class RelativePointer private(val displacement: Int) extends Operand {
  assume((displacement > -8388608) && (displacement < 8388607))

  def encode = displacement & 0xFFFFFF

  override def toString() = s"${(displacement * 4).toString()}"
}

object RelativePointer {
  implicit def apply(displacement: Int) = new RelativePointer(displacement/4)
}