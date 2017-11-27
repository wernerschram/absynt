package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.ValueSize
import assembler.{Offset, OffsetFactory, RelativeOffset}

abstract class X86Offset(val offset: Long) extends Offset {

  def isShort(shortJumpSize: Int): Boolean = offset >= Byte.MinValue && offset <= Byte.MaxValue

  def operandByteSize: ValueSize

  def encode(sourceInstructionBaseSize: Int): List[Byte]
  def encodeShort(sourceInstructionBaseSize: Int): List[Byte] = offset.toByte.encodeLittleEndian

  def add[OffsetType <: X86Offset with RelativeOffset: OffsetFactory](that: OffsetType): OffsetType =
    implicitly[OffsetFactory[OffsetType]].offset(this.offset + that.offset)
  def +[OffsetType <: X86Offset with RelativeOffset: OffsetFactory](that: OffsetType): OffsetType = add(that)

  def add[OffsetType <: X86Offset with X86RelativeOffset: OffsetFactory](that: Long): OffsetType =
    implicitly[OffsetFactory[OffsetType]].offset(this.offset + that)
  def +[OffsetType <: X86Offset with X86RelativeOffset: OffsetFactory](that: Long): OffsetType = add(that)
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
  override def encode(sourceInstructionSize: Int): List[Byte] = offset.toShort.encodeLittleEndian
}

sealed case class ProtectedRelativeOffset(override val offset: Long) extends ProtectedX86Offset(offset) with X86RelativeOffset {
  val operandByteSize: ValueSize = ValueSize.DoubleWord
  override def encode(sourceInstructionSize: Int): List[Byte] = offset.toInt.encodeLittleEndian
}

object X86RelativeOffset {
  def RealOffset(offset: Long) = RealRelativeOffset(offset)
  def ProtectedOffset(offset: Long) = ProtectedRelativeOffset(offset)
}

