package assembler.x86.operands.memoryaccess

import assembler.x86.operands.Operand
import assembler.x86.operands.FixedSizeParameter

class NearPointer private(val displacement: List[Byte]) extends Operand with FixedSizeParameter {
  assume(List(1, 2, 4).contains(displacement.length))
  val operandByteSize: Int = displacement.length
  
  def getRexRequirements(position: assembler.x86.ParameterPosition): List[assembler.x86.RexExtendedRequirement] = 
    Nil
}

object NearPointer {
  def apply(displacement: List[Byte]) = new NearPointer(displacement)
}