package assembler.x86.operations

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableOperand
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.EncodableRegister
import assembler.x86.operands.ModRMEncodableOperand

class ModRRMStaticOperation[RegisterType <: EncodableRegister](
  override val operand1: RegisterType,
  override val operand2: ModRMEncodableOperand,
  override val code: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends OneOperandOperation[RegisterType] with SecondOperand[RegisterType, ModRMEncodableOperand] {

  override val parameter1Position = ParameterPosition.OperandR
  override val parameter2Position = ParameterPosition.OperandRM

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operand2.getExtendedBytes(operand1)
  }
}
