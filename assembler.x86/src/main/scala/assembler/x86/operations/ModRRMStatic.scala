package assembler.x86.operations

import assembler.x86.operands._
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}
import assembler.x86.operations.OperandInfo.OperandOrder._

abstract class ModRRMStatic[RegisterType <: GeneralPurposeRegister](val register: RegisterType,
                                                           operandRM: ModRMEncodableOperand,
                                                           override val code: Seq[Byte],
                                                           override val mnemonic: String,
                                                           override val includeRexW: Boolean = true)
                                                          (override implicit val processorMode: ProcessorMode)
  extends ModRMStatic(operandRM, code, register.registerOrMemoryModeCode, mnemonic, includeRexW) {

  def operandROrder: OperandOrder =
    if (operandRMOrder == first) second else first

  override def operands: Seq[OperandInfo] = OperandInfo.rmRegister(register, operandROrder) +: super.operands

  override def validate(): Unit = {
    super.validate()
    assume(register.isValidForMode(processorMode))
  }

  override def rexRequirements: Seq[RexRequirement] = super.rexRequirements ++
    register.getRexRequirements(ParameterPosition.OperandR) ++
    operandRM.getRexRequirements(ParameterPosition.OperandRM)
}
