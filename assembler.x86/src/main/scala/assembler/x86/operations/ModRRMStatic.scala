package assembler.x86.operations

import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.EncodableRegister
import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.OperandSize

class ModRRMStaticOperation[RegisterType <: EncodableRegister](
  val register: RegisterType,
  operandRM: ModRMEncodableOperand,
  override val code: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends ModRMStaticOperation(operandRM, code, register.registerOrMemoryModeCode, mnemonic, includeRexW) {

  override def operands: List[Operand] = register :: super.operands

  override def validate = {
    super.validate
    assume(register.isValidForMode(processorMode))
  }

  override def operandSize: OperandSize = (super.operandSize, register) match {
    case (OperandSize.Unknown, fixed: FixedSizeOperand) => fixed.operandByteSize
    case _ => super.operandSize
  }

  override def rexRequirements = super.rexRequirements ::: register.getRexRequirements(ParameterPosition.OperandR)
}
