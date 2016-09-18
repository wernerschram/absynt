package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.operands.registers.Register

class RegisterStatic[RegisterType <: Register](code: List[Byte], includeRexW: Boolean = true)(implicit mnemonic: String)
  extends OneOperand[RegisterType](ParameterPosition.None, mnemonic) {

  def getCode(register: RegisterType) = code
}