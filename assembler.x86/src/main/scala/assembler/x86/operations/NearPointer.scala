package assembler.x86.operations

import assembler.x86.operands.ValueSize
import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerType}
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder

trait NearPointer[Size<:ValueSize] extends DisplacementBytes {

  self: X86Operation =>
  def pointer: NearPointerType with Size

  def pointerOrder: OperandOrder

  override def displacementBytes: Seq[Byte] = pointer.encodeBytes

  override protected def displacementInit(): Unit =
    addOperand(OperandInfo.relative(pointer, pointerOrder))
}
