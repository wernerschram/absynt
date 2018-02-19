package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerType}
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder

trait NearPointer extends X86Operation {

  self: X86Operation =>
  def pointer: NearPointerType

  def pointerOrder: OperandOrder

  abstract override def operands: Seq[OperandInfo] = super.operands :+ OperandInfo.relative(pointer, pointerOrder)

  override def validate(): Unit = {
    super.validate()
    assume(pointer.isValidForMode(processorMode))
  }

  abstract override def encodeByte: Seq[Byte] =
    super.encodeByte ++ pointer.encodeBytes
}
