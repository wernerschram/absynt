package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.operands.memoryaccess.{FarPointer => FarPointerType}
import assembler.x86.operands.{FarPointerSize, Operand}

trait FarPointer extends X86Operation {

  self: X86Operation =>
  def pointer: FarPointerType

  abstract override def operands: List[Operand] = super.operands ::: pointer :: Nil

  abstract override def operandSize: FarPointerSize = pointer.operandByteSize

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] =
    super.encodeByte() ::: pointer.offset ::: pointer.segment
}
