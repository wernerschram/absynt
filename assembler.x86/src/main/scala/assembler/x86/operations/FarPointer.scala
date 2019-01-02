package assembler.x86.operations

import assembler.x86.operands.FarPointerSize
import assembler.x86.operands.memoryaccess.{FarPointer => FarPointerType}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait FarPointer[Size<:FarPointerSize] extends DisplacementBytes {

  self: X86Operation =>
  def pointer: FarPointerType with Size

  override protected def displacementInit(): Unit =
    addOperand(OperandInfo.pointer(pointer, destination))

  override def displacementBytes: Seq[Byte] = pointer.encodeByte
}
