package assembler.x86.operations

import assembler.x86.operands.{ExtendedSize, FarPointerSize}
import assembler.x86.operands.memoryaccess.{FarPointer => FarPointerType}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait FarPointer[OffsetSize<:ExtendedSize] extends DisplacementBytes {

  self: X86Operation =>
  def pointer: FarPointerType[OffsetSize] with FarPointerSize[OffsetSize]

  override protected def displacementInit(): Unit =
    addOperand(OperandInfo.pointer(pointer, destination))

  override def displacementBytes: Seq[Byte] = pointer.encodeByte
}
