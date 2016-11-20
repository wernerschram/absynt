package assembler.x86.operands.memoryaccess

import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.Operand

class FarPointer(val segment: List[Byte], val offset: List[Byte]) extends Operand with FixedSizeOperand {
  assume(segment.size == 2)
  assume(List(2, 4).contains(offset.length))
  
  override val operandByteSize: Int = offset.length
  
  def getRexRequirements(position: assembler.x86.ParameterPosition): List[assembler.x86.RexExtendedRequirement] = 
    Nil
}