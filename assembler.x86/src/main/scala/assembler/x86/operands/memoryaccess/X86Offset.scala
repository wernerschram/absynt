package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.ValueSize
import assembler.{Offset, OffsetDirection, OffsetFactory}

sealed abstract class X86Offset(val offset: Long) extends Offset {

  def isShort(shortJumpSize: Int): Boolean = offset >= Byte.MinValue && offset <= Byte.MaxValue

  def operandByteSize: ValueSize

  def encode(sourceInstructionBaseSize: Int): List[Byte]
  def encodeShort(sourceInstructionBaseSize: Int): List[Byte] = offset.toByte.encodeLittleEndian


  def add[OffsetType <: X86Offset: OffsetFactory](that: OffsetType): OffsetType =
    implicitly[OffsetFactory[OffsetType]].offset(this.offset + that.offset)
  def +[OffsetType <: X86Offset: OffsetFactory](that: OffsetType): OffsetType = add(that)

  def add[OffsetType <: X86Offset: OffsetFactory](that: Long): OffsetType =
    implicitly[OffsetFactory[OffsetType]].offset(this.offset + that)
  def +[OffsetType <: X86Offset: OffsetFactory](that: Long): OffsetType = add(that)

  override def direction: OffsetDirection =
      if (offset == 0)
        OffsetDirection.None
      else if (offset < 0)
        OffsetDirection.Backward
      else
        OffsetDirection.Forward
}

sealed case class RealOffset(override val offset: Long) extends X86Offset(offset) {
  assert(offset>=Short.MinValue)
  assert(offset<=Short.MaxValue)
  val operandByteSize: ValueSize = ValueSize.Word
  override def encode(sourceInstructionSize: Int): List[Byte] = offset.toShort.encodeLittleEndian
}

sealed case class ProtectedOffset(override val offset: Long) extends X86Offset(offset) {
  val operandByteSize: ValueSize = ValueSize.DoubleWord
  override def encode(sourceInstructionSize: Int): List[Byte] = offset.toInt.encodeLittleEndian
}

object X86Offset {
  def RealOffset(offset: Long) = new RealOffset(offset)
  def ProtectedOffset(offset: Long) = new ProtectedOffset(offset)
}

