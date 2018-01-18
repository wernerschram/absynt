package assembler.x86.operations

import assembler.Label
import assembler.x86.ProcessorMode
import assembler.x86.operands.{ModRMEncodableOperand, Operand, SegmentRegister}

class ModSegmentRMStatic(val register: SegmentRegister,
                         operandRM: ModRMEncodableOperand,
                         override val code: Seq[Byte],
                         override val mnemonic: String,
                         override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
  extends ModRMStatic(operandRM, code, register.registerCode, mnemonic, includeRexW) {

  override def operands: Seq[Operand] = register +: super.operands

  override def validate(): Unit = {
    super.validate()
    assume(register.isValidForMode(processorMode))
  }
}
