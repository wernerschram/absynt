package assembler.x86.operations

import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}
import assembler.x86.operands.Operand
import assembler.x86.operands.GeneralPurposeRegister

class RegisterEncoded[RegisterType <: GeneralPurposeRegister](
  register: RegisterType,
  rawCode: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends X86Operation {

  override def operands: List[Operand] = register :: Nil

  override def rexRequirements: List[RexRequirement] = super.rexRequirements ::: register.getRexRequirements(ParameterPosition.OpcodeReg)

  override def code: List[Byte] = {
    rawCode.take(rawCode.length - 1) ::: (rawCode.last | register.registerCode).toByte :: Nil
  }
}
