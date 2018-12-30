package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._

abstract class ModRRM[RegisterType <: GeneralPurposeRegister](val register: RegisterType,
                                                              operandRM: ModRMEncodableOperand,
                                                              override val code: Seq[Byte],
                                                              override val mnemonic: String,
                                                              override val operandRMOrder: OperandOrder)
                                                             (override implicit val processorMode: ProcessorMode)
  extends ModRM(operandRM, code, register.registerOrMemoryModeCode, mnemonic, operandRMOrder) {
  self: X86Operation with DisplacementBytes with ImmediateBytes =>

  def operandROrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override protected def modRMInit(): Unit = {
    super.modRMInit()
    addOperand(OperandInfo.rmRegister(register, operandROrder))
  }
}
