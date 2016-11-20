package assembler.x86.operations

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableOperand
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.EncodableRegister
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.x86.operands.SegmentRegister
import assembler.x86.instructions.FixedSizeX86Operation2

class ModSegmentRMStaticOperation (
  val register: SegmentRegister,
  operand1: ModRMEncodableOperand,
  override val code: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends ModRMStaticOperation(operand1, code, register.registerCode, mnemonic, includeRexW) {

  override def operands: List[Operand] = register :: super.operands

  override def validate = {
    super.validate
    assume(register.isValidForMode(processorMode))
  }
}
