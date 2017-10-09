package assembler.arm.operands

import assembler.{Address, Offset}
import assembler.ListExtensions._

import scala.language.implicitConversions

sealed class ArmOffset private(val offset: Int) extends Offset {

  override def toString: String = s"0x${offset.encodeBigEndian.hexString}"

  def add(that: ArmOffset): ArmOffset = ArmOffset(offset + that.offset)
  def +(that: ArmOffset): ArmOffset = add(that)
}

object ArmOffset {
  def apply(offset: Int) = new ArmOffset(offset)
}

sealed abstract class RelativePointer(val offset: ArmOffset) extends Address[ArmOffset] with Operand {
  //offset should be between 3221225472 and -3221225473
  assume(((offset.offset & 0xC0000000) == 0) || ((offset.offset & 0xC0000000) == 0xC0000000))

  def encode: Int = (offset.offset >> 2) & 0xFFFFFF

  override def toString: String = offset.toString
}

class RelativeA32Pointer private(offset: ArmOffset) extends RelativePointer(offset) {
  //offset should be divisible by 4
  assume((offset.offset & 0x00000003) == 0)

    override def add(that: ArmOffset) = RelativeA32Pointer(offset + that)
}

class RelativeThumbPointer private(offset: ArmOffset) extends RelativePointer(offset) {
  //offset should be divisible by 2
  assume((offset.offset & 0x00000001) == 0)

  override def add(that: ArmOffset) = RelativeThumbPointer(offset + that)

  override def encode: Int = super.encode | ((offset.offset & 2) << 23)
}

object RelativeA32Pointer {
  implicit def apply(offset: ArmOffset): RelativeA32Pointer = new RelativeA32Pointer(offset)
}

object RelativeThumbPointer {
  implicit def apply(offset: ArmOffset): RelativeThumbPointer = new RelativeThumbPointer(offset)
}
