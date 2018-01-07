package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{X86Offset, FarPointer => FarPointerType}
import assembler.x86.operands.{FarPointerSize, Operand}

trait FarPointer[OffsetType <: X86Offset] extends X86Operation {

  self: X86Operation =>
  def pointer: FarPointerType[OffsetType]

  abstract override def operands: List[Operand] = super.operands ::: pointer :: Nil

  abstract override def operandSize: FarPointerSize = pointer.operandByteSize

  abstract override def encodeByte: Seq[Byte] =
    super.encodeByte ++ pointer.encodeByte
}
