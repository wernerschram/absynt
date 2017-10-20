package assembler.arm.operands

import assembler.{Address, Offset, OffsetDirection}
import assembler.ListExtensions._

import scala.language.implicitConversions

sealed case class ArmOffset private(offset: Int) extends Offset {

  override def toString: String = s"0x${offset.encodeBigEndian.hexString}"

  def add(that: ArmOffset): ArmOffset = ArmOffset(offset + that.offset)
  def +(that: ArmOffset): ArmOffset = add(that)

  def add(that: Long): ArmOffset = ArmOffset((offset + that).toInt)
  def +(that: Long): ArmOffset = add(that)

  override def direction: OffsetDirection =
    if (offset == 0)
      OffsetDirection.None
    else if (offset < 0)
      OffsetDirection.Backward
    else
      OffsetDirection.Forward
}

sealed abstract class RelativePointer(val offset: ArmOffset) extends Address[ArmOffset] with Operand {
  //offset should be between 3221225472 and -3221225473
  assume(((offset.offset & 0xC0000000) == 0) || ((offset.offset & 0xC0000000) == 0xC0000000))

  def encode: Int = (offset.offset >> 2) & 0xFFFFFF

  override def toString: String = offset.toString
}

case class RelativeA32Pointer(override val offset: ArmOffset) extends RelativePointer(offset) {
  //offset should be divisible by 4
  assume((offset.offset & 0x00000003) == 0)

  override def toLong = offset.offset
}

case class RelativeThumbPointer(override val offset: ArmOffset) extends RelativePointer(offset) {
  //offset should be divisible by 2
  assume((offset.offset & 0x00000001) == 0)

  override def encode: Int = super.encode | ((offset.offset & 2) << 23)

  override def toLong = offset.offset
}

