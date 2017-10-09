package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{X86Offset, NearPointer => NearPointerType}
import assembler.x86.operands.{Operand, OperandSize}

trait NearPointer[OffsetType <: X86Offset] extends X86Operation {

  self: X86Operation =>
  def pointer: NearPointerType[OffsetType]

  abstract override def operands: List[Operand] = super.operands ::: pointer :: Nil

  abstract override def operandSize: OperandSize = pointer.operandByteSize

  override def validate(): Unit = {
    super.validate()
    assume(pointer.isValidForMode(processorMode))
  }

  abstract override def encodeByte: List[Byte] =
    super.encodeByte ::: pointer.encodeBytes
}
