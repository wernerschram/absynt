package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._

class ModRRM[RegisterType <: GeneralPurposeRegister](val register: RegisterType,
                                                              operandRM: ModRMEncodableOperand,
                                                              override val code: Seq[Byte],
                                                              override val mnemonic: String,
                                                              override val operandRMOrder: OperandOrder)
                                                             (override implicit val processorMode: ProcessorMode)
  extends ModRM(operandRM, code, register.registerOrMemoryModeCode, mnemonic, operandRMOrder) {

  def operandROrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override def operands: Set[OperandInfo] = super.operands + OperandInfo.rmRegister(register, operandROrder)
}
