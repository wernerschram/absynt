package assembler.arm.operands

import assembler.Address

import scala.language.implicitConversions

object ArmOffset {
  type offset = Int
}

sealed class RelativePointer(val offset: ArmOffset.offset) extends Address[ArmOffset.offset] with Operand {
  //offset should be between 3221225472 and -3221225473
  assume(((offset & 0xC0000000) == 0) || ((offset & 0xC0000000) == 0xC0000000))

  def encode: Int = (offset >> 2) & 0xFFFFFF

  override def toString: String = offset.toString

  override def add(that: ArmOffset.offset) = RelativeA32Pointer(offset + that)
}

class RelativeA32Pointer private(displacement: ArmOffset.offset) extends RelativePointer(displacement) {
  //offset should be divisible by 4
  assume((displacement & 0x00000003) == 0)
}

class RelativeThumbPointer private(displacement: ArmOffset.offset) extends RelativePointer(displacement) {
  //offset should be divisible by 4
  assume((displacement & 0x00000002) == 0)

  override def encode: Int = super.encode | ((displacement & 1) << 24)
}

object RelativeA32Pointer {
  implicit def apply(displacement: ArmOffset.offset): RelativeA32Pointer = new RelativeA32Pointer(displacement)
}

object RelativeThumbPointer {
  implicit def apply(displacement: ArmOffset.offset): RelativeThumbPointer = new RelativeThumbPointer(displacement)
}
