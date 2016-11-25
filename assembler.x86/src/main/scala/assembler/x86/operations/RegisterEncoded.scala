package assembler.x86.operations

import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.EncodableRegister
import assembler.x86.operands.Operand

class RegisterEncoded[RegisterType <: EncodableRegister](
  register: RegisterType,
  rawCode: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends X86Operation {

  override def operands: List[Operand] = register :: Nil

  override def rexRequirements = super.rexRequirements ::: register.getRexRequirements(ParameterPosition.OpcodeReg)

  override def code: List[Byte] = {
    rawCode.take(rawCode.length - 1) ::: (rawCode.last | register.registerCode).toByte :: Nil
  }
}
