package assembler.x86.operations

import assembler.x86.operands.EncodableOperand
import assembler.x86.ParameterPosition
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.Operand

trait Immediate[T <: Operand] extends SecondOperand[T, ImmediateValue] {

  self: OneOperandOperation[T] =>
  override val parameter1Position = ParameterPosition.OperandRM
  override val parameter2Position = ParameterPosition.NotEncoded

//  override def validate(operand: EncodableOperand, immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Boolean =
//    super.validate(operand, immediate) && validateExtension(operand, immediate, processorMode)


  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operand2.value
  }

}
