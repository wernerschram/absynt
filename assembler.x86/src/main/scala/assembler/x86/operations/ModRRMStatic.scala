package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operations.OperandInfo.OperandOrder._

abstract class ModRRMStatic[RegisterType <: GeneralPurposeRegister](val register: RegisterType,
                                                           operandRM: ModRMEncodableOperand,
                                                           override val code: Seq[Byte],
                                                           override val mnemonic: String,
                                                           override val includeRexW: Boolean = true)
                                                          (override implicit val processorMode: ProcessorMode)
  extends ModRMStatic(operandRM, code, register.registerOrMemoryModeCode, mnemonic, includeRexW) {

  def operandROrder: OperandOrder =
    if (operandRMOrder == destination) source else destination

  override def operands: Set[OperandInfo] = super.operands + OperandInfo.rmRegister(register, operandROrder)
}
