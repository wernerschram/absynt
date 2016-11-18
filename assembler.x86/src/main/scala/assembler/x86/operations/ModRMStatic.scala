package assembler.x86.operations

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableOperand
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage

class ModRMStaticOperation(
  override val operand1: EncodableOperand,
  override val code: List[Byte],
  val rValue: Byte,
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends OneOperandOperation[EncodableOperand] {
//  assume(validate)

  override val parameter1Position = ParameterPosition.OperandRM

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operand1.getExtendedBytes(rValue)
  }
}
