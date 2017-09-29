package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.Address
import assembler.x86.operands.{FarPointerSize, FixedSizeOperand, Operand}

class FarPointer(val segment: Short, val offset: LongOffset) extends Operand with Address[LongOffset] with FixedSizeOperand {

  override val operandByteSize: FarPointerSize = offset.farPointerSize

  override def toString = s"FAR 0x${segment.encodeLittleEndian.bigEndianHexString}:$offset"

  def encodeByte: List[Byte] = offset.encodeByte ::: segment.encodeLittleEndian

  override def add(offset: LongOffset): Nothing = ???
}

object FarPointer {
  def apply(segment: Short, Offset: LongOffset): FarPointer = new FarPointer(segment, Offset)
}