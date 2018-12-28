package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands.GeneralPurposeRegister
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder

abstract class RegisterEncoded[RegisterType <: GeneralPurposeRegister](register: RegisterType,
                                                              rawCode: Seq[Byte],
                                                              override val mnemonic: String)
                                                             (override implicit val processorMode: ProcessorMode)
  extends X86Operation {

  def registerOrder: OperandOrder

  override def operands: Set[OperandInfo] = Set(OperandInfo.encodedRegister(register, registerOrder))

  override def code: Seq[Byte] =
    rawCode.take(rawCode.length - 1) :+ (rawCode.last | register.registerCode).toByte
}
