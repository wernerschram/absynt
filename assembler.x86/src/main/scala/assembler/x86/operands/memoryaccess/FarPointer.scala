package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.OperandSize
import assembler.x86.ParameterPosition
import assembler.x86.RexExtendedRequirement

class FarPointer(val segment: List[Byte], val offset: List[Byte]) extends Operand with FixedSizeOperand {
  assume(segment.size == 2)
  assume(List(2, 4).contains(offset.length))

  override val operandByteSize = OperandSize(offset.length)

  override def toString() = s"FAR 0x${segment.bigEndianHexString()}:0x${offset.bigEndianHexString()}"
}