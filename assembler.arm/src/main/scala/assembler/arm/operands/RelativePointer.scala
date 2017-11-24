package assembler.arm.operands

import assembler.{Address, Offset, OffsetDirectionOld, RelativeOffset}
import assembler.ListExtensions._

import scala.language.implicitConversions

abstract class ArmOffset protected(val offset: Int) extends Offset {
  override def toString: String = s"0x${offset.encodeBigEndian.hexString}"

  def add(that: ArmOffset with RelativeOffset): ArmOffset with RelativeOffset = ArmRelativeOffset(offset + that.offset)
  def +(that: ArmOffset with RelativeOffset): ArmOffset with RelativeOffset = add(that)

  def add(that: Long): ArmOffset with RelativeOffset = ArmRelativeOffset((offset + that).toInt)
  def +(that: Long): ArmOffset with RelativeOffset = add(that)

}

sealed case class ArmRelativeOffset private(override val offset: Int) extends ArmOffset(offset) with RelativeOffset {
  override def direction: OffsetDirectionOld =
    if (offset == 0)
      OffsetDirectionOld.None
    else if (offset < 0)
      OffsetDirectionOld.Backward
    else
      OffsetDirectionOld.Forward
}

sealed abstract class RelativePointer(val offset: ArmOffset with RelativeOffset) extends Address[ArmOffset] with Operand {
  //offset should be between 3221225472 and -3221225473
  assume(((offset.offset & 0xC0000000) == 0) || ((offset.offset & 0xC0000000) == 0xC0000000))

  def encode: Int = (offset.offset >> 2) & 0xFFFFFF

  override def toString: String = offset.toString
}

case class RelativeA32Pointer(override val offset: ArmOffset with RelativeOffset) extends RelativePointer(offset) {
  //offset should be divisible by 4
  assume((offset.offset & 0x00000003) == 0)

  override def toLong: Long = offset.offset
}

case class RelativeThumbPointer(override val offset: ArmOffset with RelativeOffset) extends RelativePointer(offset) {
  //offset should be divisible by 2
  assume((offset.offset & 0x00000001) == 0)

  override def encode: Int = super.encode | ((offset.offset & 2) << 23)

  override def toLong: Long = offset.offset
}

