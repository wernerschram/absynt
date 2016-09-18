package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.operands.registers.EncodableRegister

class RegisterEncoded[RegisterType <: EncodableRegister](code: List[Byte], override val includeRexW: Boolean = true)(implicit mnemonic: String) 
  extends OneOperand[RegisterType](ParameterPosition.OpcodeReg, mnemonic) {

  override def getCode(operand: RegisterType): List[Byte] = 
    code.take(code.length - 1) ::: (code.last | operand.registerOrMemoryModeCode).toByte :: Nil
}