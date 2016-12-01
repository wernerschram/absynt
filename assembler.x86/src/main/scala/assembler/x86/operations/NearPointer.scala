package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.operands.memoryaccess.{ NearPointer => NearPointerType }
import assembler.x86.operands.OperandSize

trait NearPointer extends X86Operation {

  self: X86Operation =>
  def pointer: NearPointerType

  abstract override def operands = super.operands ::: pointer :: Nil

  abstract override def operandSize: OperandSize = pointer.operandByteSize

  override def validate = {
    super.validate
    assume(pointer.isValidForMode(processorMode))
  }

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] =
    super.encodeByte() ::: pointer.displacement
}
