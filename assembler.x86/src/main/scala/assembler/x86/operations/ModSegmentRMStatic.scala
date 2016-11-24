package assembler.x86.operations

import assembler.x86.ProcessorMode
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.SegmentRegister

class ModSegmentRMStaticOperation (
  val register: SegmentRegister,
  operandRM: ModRMEncodableOperand,
  override val code: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends ModRMStaticOperation(operandRM, code, register.registerCode, mnemonic, includeRexW) {

  override def operands: List[Operand] = register :: super.operands

  override def validate = {
    super.validate
    assume(register.isValidForMode(processorMode))
  }
}
