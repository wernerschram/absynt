package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.EncodableRegister

class ModRRMStatic[RegisterType <: EncodableRegister](code: List[Byte], override val includeRexW: Boolean = true)(implicit mnemonic: String)
  extends TwoOperand[RegisterType, ModRMEncodableOperand](ParameterPosition.OperandR, ParameterPosition.OperandRM, mnemonic) {

  def getCode(registerR: RegisterType, operandRM: ModRMEncodableOperand): List[Byte] =
      code ::: operandRM.getExtendedBytes(registerR)
}

