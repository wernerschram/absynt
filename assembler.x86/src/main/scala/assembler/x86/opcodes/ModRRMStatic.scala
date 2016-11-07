package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.EncodableRegister

class ModRRMStatic[RegisterType <: EncodableRegister](code: List[Byte], override val includeRexW: Boolean = true)(implicit val mnemonic: String)
  extends TwoOperand[RegisterType, ModRMEncodableOperand]{

  val parameter1Position = ParameterPosition.OperandR
  val parameter2Position = ParameterPosition.OperandRM

  def getCode(registerR: RegisterType, operandRM: ModRMEncodableOperand): List[Byte] =
      code ::: operandRM.getExtendedBytes(registerR)
}

