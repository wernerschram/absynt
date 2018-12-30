package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerType}
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder

trait NearPointer extends DisplacementBytes {

  self: X86Operation =>
  def pointer: NearPointerType

  def pointerOrder: OperandOrder

  override def displacementBytes: Seq[Byte] = pointer.encodeBytes

  override protected def displacementInit(): Unit =
    addOperand(OperandInfo.relative(pointer, pointerOrder))
}
