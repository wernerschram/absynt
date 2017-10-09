package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.Offset
import assembler.x86.{OffsetFactory, ProcessorModeWithOffset}
import assembler.x86.operands.ValueSize

sealed abstract class X86Offset(val offset: Long) extends Offset {
  def isShort: Boolean = offset.toByte == offset
  def operandByteSize: ValueSize

  def encode: List[Byte]
  def encodeShort: List[Byte] = offset.toByte.encodeLittleEndian


  def add[OffsetType <: X86Offset: OffsetFactory](that: OffsetType): OffsetType =
    implicitly[OffsetFactory[OffsetType]].offset(this.offset + that.offset)
  def +[OffsetType <: X86Offset: OffsetFactory](that: OffsetType): OffsetType = add(that)
}

trait X86OffsetFactory[OffsetType <: X86Offset] {
  def build(offset: Long): OffsetType
}

sealed class RealOffset(offset: Long) extends X86Offset(offset) {
  assert(offset>=Short.MinValue)
  assert(offset<=Short.MaxValue)
  val operandByteSize: ValueSize = ValueSize.Word
  override def encode: List[Byte] = offset.toShort.encodeLittleEndian
}

sealed class ProtectedOffset(offset: Long) extends X86Offset(offset) {
  val operandByteSize: ValueSize = ValueSize.DoubleWord
  override def encode: List[Byte] = offset.toInt.encodeLittleEndian
}

object X86Offset {
  def RealOffset(offset: Long) = new RealOffset(offset)
  def ProtectedOffset(offset: Long) = new ProtectedOffset(offset)
}

