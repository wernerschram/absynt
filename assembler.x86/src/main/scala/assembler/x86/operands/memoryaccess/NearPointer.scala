package assembler.x86.operands.memoryaccess

import assembler.{Address, Offset}
import assembler.ListExtensions._
import assembler.x86.operands.{FixedSizeOperand, Operand, OperandSize, ValueSize}

class NearPointer private(val offset: List[Byte]) extends Operand with Address with FixedSizeOperand {
  assume(List(1, 2, 4).contains(offset.length))
  val operandByteSize: OperandSize = ValueSize.sizeOfValue(offset.length)

  override def toString = s"0x${offset.bigEndianHexString}"

  def encodeBytes = offset

  override def add(offset: Offset) = ???
}

object NearPointer {
  def apply(displacement: List[Byte]) = new NearPointer(displacement)
}