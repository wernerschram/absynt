package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableOperand

class ModRMStatic(code: List[Byte], val rValue: Byte = 0, override val includeRexW: Boolean = true)(implicit val mnemonic: String)
    extends OneOperand[EncodableOperand] {
  val parameterPosition = ParameterPosition.OperandRM

  def getCode(operandRM: EncodableOperand) = code ::: operandRM.getExtendedBytes(rValue)
}