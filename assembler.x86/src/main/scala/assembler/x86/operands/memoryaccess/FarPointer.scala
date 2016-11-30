package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.OperandSize
import assembler.x86.ParameterPosition
import assembler.x86.RexExtendedRequirement
import assembler.x86.operands.FarPointerSize

class FarPointer(val segment: List[Byte], val offset: List[Byte]) extends Operand with FixedSizeOperand {

  override val operandByteSize = FarPointerSize.sizeOfFarPointer(segment.length, offset.length)

  override def toString() = s"FAR 0x${segment.bigEndianHexString()}:0x${offset.bigEndianHexString()}"
}