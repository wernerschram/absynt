package assembler.x86.operands.memoryaccess

import assembler.Offset
import assembler.ListExtensions._
import assembler.x86.operands.{FarPointerSize, ValueSize}
import assembler.x86.operands.ValueSize.{Byte, DoubleWord, Word}

trait X86Offset extends Offset {
  def size: ValueSize
  def encodeByte: List[Byte]
  override def toString: String = s"0x${encodeByte.bigEndianHexString}"
}

sealed class ShortOffset(value: Byte) extends X86Offset {
  override def size: ValueSize = Byte

  override def encodeByte: List[Byte] = value.encodeLittleEndian
}

trait LongOffset extends X86Offset {
  def farPointerSize: FarPointerSize
}

sealed class RealModeLongOffset(value: Short) extends LongOffset {
  override def size: ValueSize = Word
  override def farPointerSize: FarPointerSize = FarPointerSize.DoubleWord
  override def encodeByte: List[Byte] = value.encodeLittleEndian
}

sealed class ProtectedModeLongOffset(value: Int) extends LongOffset {
  override def size: ValueSize = DoubleWord
  override def farPointerSize: FarPointerSize = FarPointerSize.FarWord
  override def encodeByte: List[Byte] = value.encodeLittleEndian
}

object X86Offset {
  implicit def apply(value: Byte): ShortOffset = new ShortOffset(value)
  implicit def apply(value: Short): RealModeLongOffset = new RealModeLongOffset(value)
  implicit def apply(value: Int): ProtectedModeLongOffset = new ProtectedModeLongOffset(value)
}
