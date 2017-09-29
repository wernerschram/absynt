package assembler.x86.operands.memoryaccess

import assembler.Offset
import assembler.ListExtensions._
import assembler.x86.operands.ValueSize
import assembler.x86.operands.ValueSize.{Byte, DoubleWord, Word}

trait X86Offset extends Offset {
  def size: ValueSize
  def encodeByte: List[Byte]
  override def toString: String = s"0x${encodeByte.bigEndianHexString}"
}

sealed class ShortOffset private(value: Byte) extends X86Offset {
  override def size: ValueSize = Byte
  override def encodeByte: List[Byte] = value.encodeLittleEndian
}

sealed class RealModeLongOffset private(value: Short) extends X86Offset {
  override def size: ValueSize = Word
  override def encodeByte: List[Byte] = value.encodeLittleEndian
}

sealed class ProtectedModeLongOffset private(value: Int) extends X86Offset {
  override def size: ValueSize = DoubleWord
  override def encodeByte: List[Byte] = value.encodeLittleEndian
}

object ShortOffset {
  def apply(value: Byte): ShortOffset = new ShortOffset(value)
}

object RealModeLongOffset {
  def apply(value: Short): RealModeLongOffset = new RealModeLongOffset(value)
}

object ProtectedModeLongOffset {
  def apply(value: Int): ProtectedModeLongOffset = new ProtectedModeLongOffset(value)
}
