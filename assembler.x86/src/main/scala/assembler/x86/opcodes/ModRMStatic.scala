package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableOperand
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operations.OneOperandOperation

class ModRMStaticWithImmediate(code: List[Byte], val rValue: Byte = 0,
  validateExtension: PartialFunction[(EncodableOperand, ImmediateValue, ProcessorMode), Boolean] = TwoOperand.valid, includeRexW: Boolean = true)(implicit val mnemonic: String)
    extends TwoOperand[EncodableOperand, ImmediateValue] {
  override val parameter1Position = ParameterPosition.OperandRM
  override val parameter2Position = ParameterPosition.NotEncoded

  override def validate(operand: EncodableOperand, immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Boolean =
    super.validate(operand, immediate) && validateExtension(operand, immediate, processorMode)

  override def getCode(operandRM: EncodableOperand, immediate: ImmediateValue): List[Byte] =
    code ::: operandRM.getExtendedBytes(rValue) ::: immediate.value
}
