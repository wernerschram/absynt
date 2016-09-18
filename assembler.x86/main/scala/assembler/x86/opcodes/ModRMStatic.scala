package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableOperand

class ModRMStatic(code: List[Byte], val rValue: Byte = 0, override val includeRexW: Boolean = true)(implicit mnemonic: String) 
  extends OneOperand[EncodableOperand](ParameterPosition.OperandRM, mnemonic) {

  def getCode(operandRM: EncodableOperand) = code ::: operandRM.getExtendedBytes(rValue) 
}