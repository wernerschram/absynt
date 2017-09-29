package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerType}
import assembler.x86.operands.{Operand, OperandSize}

trait NearPointer extends X86Operation {

  self: X86Operation =>
  def pointer: NearPointerType

  abstract override def operands: List[Operand] = super.operands ::: pointer :: Nil

  abstract override def operandSize: OperandSize = pointer.operandByteSize

  override def validate(): Unit = {
    super.validate()
    assume(pointer.isValidForMode(processorMode))
  }

  abstract override def encodeByte: List[Byte] =
    super.encodeByte ::: pointer.offset.encodeByte
}
