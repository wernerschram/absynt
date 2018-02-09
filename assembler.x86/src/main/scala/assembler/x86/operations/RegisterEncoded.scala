package assembler.x86.operations

import assembler.x86.operands.GeneralPurposeRegister
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

class RegisterEncoded[RegisterType <: GeneralPurposeRegister](register: RegisterType,
                                                              rawCode: Seq[Byte],
                                                              override val mnemonic: String,
                                                              override val includeRexW: Boolean = true)
                                                             (override implicit val processorMode: ProcessorMode)
  extends X86Operation {

  override def operands: Seq[OperandInfo] = Seq(OperandInfo.encodedRegister(register))

  override def rexRequirements: Seq[RexRequirement] = super.rexRequirements ++ register.getRexRequirements(ParameterPosition.OpcodeReg)

  override def code: Seq[Byte] =
    rawCode.take(rawCode.length - 1) :+ (rawCode.last | register.registerCode).toByte
}
