package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands.GeneralPurposeRegister
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder

abstract class RegisterEncoded[RegisterType <: GeneralPurposeRegister](register: RegisterType,
                                                              rawCode: Seq[Byte],
                                                              override val mnemonic: String)
                                                             (override implicit val processorMode: ProcessorMode)
  extends X86Operation(rawCode.take(rawCode.length - 1) :+ (rawCode.last | register.registerCode).toByte) with NoModRM {

  self: DisplacementBytes with ImmediateBytes =>
  def registerOrder: OperandOrder

  override protected def implicitInit(): Unit =
    addOperand(OperandInfo.encodedRegister(register, registerOrder))
}
