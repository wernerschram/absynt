package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.{Address, Offset}
import assembler.x86.operands.{FarPointerSize, FixedSizeOperand, Operand}

class FarPointer(val segment: List[Byte], val offset: List[Byte]) extends Operand with Address with FixedSizeOperand {

  override val operandByteSize: FarPointerSize = FarPointerSize.sizeOfFarPointer(segment.length, offset.length)

  override def toString = s"FAR 0x${segment.bigEndianHexString}:0x${offset.bigEndianHexString}"

  override def add(offset: Offset) = ???
}