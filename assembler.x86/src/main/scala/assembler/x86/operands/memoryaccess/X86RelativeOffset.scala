package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.ValueSize
import assembler.{Offset, OffsetDirection, RelativeOffset, RelativeOffsetDirection}

abstract class X86Offset(val offset: Long) extends Offset {

  def isShort(shortJumpSize: Int): Boolean = offset >= Byte.MinValue && offset <= Byte.MaxValue

  def operandByteSize: ValueSize

  def encode(sourceInstructionBaseSize: Int): Seq[Byte]
  def encodeShort(sourceInstructionBaseSize: Int): Seq[Byte] = offset.toByte.encodeLittleEndian
}

abstract class RealX86Offset(offset: Long) extends X86Offset(offset)
abstract class ProtectedX86Offset(offset: Long) extends X86Offset(offset)

sealed trait X86RelativeOffset extends RelativeOffset {
  self: X86Offset =>
}

sealed case class RealRelativeOffset(override val offset: Long) extends RealX86Offset(offset) with X86RelativeOffset {
  assert(offset>=Short.MinValue)
  assert(offset<=Short.MaxValue)
  val operandByteSize: ValueSize = ValueSize.Word
  override def encode(sourceInstructionSize: Int): Seq[Byte] = offset.toShort.encodeLittleEndian
}

sealed case class ProtectedRelativeOffset(override val offset: Long) extends ProtectedX86Offset(offset) with X86RelativeOffset {
  val operandByteSize: ValueSize = ValueSize.DoubleWord
  override def encode(sourceInstructionSize: Int): Seq[Byte] = offset.toInt.encodeLittleEndian
}

object X86RelativeOffset {
  def RealOffset(offset: Long) = RealRelativeOffset(offset)
  def ProtectedOffset(offset: Long) = ProtectedRelativeOffset(offset)

  implicit def realPositionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection)(instructionSize: Int): RealX86Offset with RelativeOffset =
    offsetDirection match {
      case OffsetDirection.Self => RealRelativeOffset(-instructionSize)
      case OffsetDirection.Forward => RealRelativeOffset(offsetValue)
      case OffsetDirection.Backward => RealRelativeOffset(-offsetValue - instructionSize)
    }

  implicit def protectedPositionalOffset(offsetValue: Long)(offsetDirection: RelativeOffsetDirection)(instructionSize: Int): ProtectedX86Offset with RelativeOffset =
    offsetDirection match {
      case OffsetDirection.Self => ProtectedRelativeOffset(-instructionSize)
      case OffsetDirection.Forward => ProtectedRelativeOffset(offsetValue)
      case OffsetDirection.Backward => ProtectedRelativeOffset(-offsetValue - instructionSize)
    }

}

