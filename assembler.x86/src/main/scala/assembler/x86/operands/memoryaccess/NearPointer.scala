package assembler.x86.operands.memoryaccess

import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.Operand

class NearPointer private(val displacement: List[Byte]) extends Operand with FixedSizeOperand {
  assume(List(1, 2, 4).contains(displacement.length))
  val operandByteSize: Int = displacement.length
  
  def getRexRequirements(position: assembler.x86.ParameterPosition): List[assembler.x86.RexExtendedRequirement] = 
    Nil
}

object NearPointer {
  def apply(displacement: List[Byte]) = new NearPointer(displacement)
}