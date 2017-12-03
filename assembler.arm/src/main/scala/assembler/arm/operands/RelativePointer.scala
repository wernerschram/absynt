package assembler.arm.operands

import assembler.ListExtensions._
import assembler.{Offset, OffsetDirection, RelativeOffset, RelativeOffsetDirection}

import scala.language.implicitConversions

abstract class ArmOffset protected(val offset: Int) extends Offset {
  override def toString: String = s"0x${(offset + 8).encodeBigEndian.hexString}"
}

sealed case class ArmRelativeOffset private(override val offset: Int) extends ArmOffset(offset) with RelativeOffset

object ArmRelativeOffset {
  implicit def positionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection): ArmOffset with RelativeOffset =
    offsetDirection match {
      case OffsetDirection.Self => ArmRelativeOffset(-8)
      case OffsetDirection.Forward => ArmRelativeOffset((offsetValue - 4).toInt)
      case OffsetDirection.Backward => ArmRelativeOffset((-offsetValue - 8).toInt)
    }

}

sealed abstract class RelativePointer(val offset: ArmOffset with RelativeOffset) extends Operand {
  //offset should be between 3221225472 and -3221225473
  assume(((offset.offset & 0xC0000000) == 0) || ((offset.offset & 0xC0000000) == 0xC0000000))

  def encode: Int = (offset.offset >> 2) & 0xFFFFFF

  override def toString: String = offset.toString
}

case class RelativeA32Pointer(override val offset: ArmOffset with RelativeOffset) extends RelativePointer(offset) {
  //offset should be divisible by 4
  assume((offset.offset & 0x00000003) == 0)
}

case class RelativeThumbPointer(override val offset: ArmOffset with RelativeOffset) extends RelativePointer(offset) {
  //offset should be divisible by 2
  assume((offset.offset & 0x00000001) == 0)

  override def encode: Int = super.encode | ((offset.offset & 2) << 23)
}

