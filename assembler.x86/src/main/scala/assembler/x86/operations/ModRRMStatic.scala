package assembler.x86.operations

import assembler.Label
import assembler.x86.operands._
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

class ModRRMStatic[RegisterType <: GeneralPurposeRegister](label: Label,
                                                           val register: RegisterType,
                                                           operandRM: ModRMEncodableOperand,
                                                           override val code: List[Byte],
                                                           override val mnemonic: String,
                                                           override val includeRexW: Boolean = true)
                                                          (override implicit val processorMode: ProcessorMode)
  extends ModRMStatic(label, operandRM, code, register.registerOrMemoryModeCode, mnemonic, includeRexW) {

  override def operands: List[Operand] = register :: super.operands

  override def validate(): Unit = {
    super.validate()
    assume(register.isValidForMode(processorMode))
  }

  override def operandSize: OperandSize = (super.operandSize, register) match {
    case (OperandSize.Unknown, fixed: FixedSizeOperand) => fixed.operandByteSize
    case _ => super.operandSize
  }

  override def rexRequirements: Seq[RexRequirement] = super.rexRequirements ++
    register.getRexRequirements(ParameterPosition.OperandR) ++
    operandRM.getRexRequirements(ParameterPosition.OperandRM)
}
