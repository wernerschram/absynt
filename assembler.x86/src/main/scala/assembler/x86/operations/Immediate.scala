package assembler.x86.operations

import assembler.x86.operands.{ImmediateValue, ValueSize}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait Immediate[Size<:ValueSize] extends ImmediateBytes {

  self: X86Operation =>
  def immediate: ImmediateValue with Size
  def immediateOrder: OperandOrder

  override protected def immediateInit(): Unit =
    addOperand(OperandInfo.immediate(immediate, immediateOrder))

  override def immediateBytes: Seq[Byte] = immediate.value
}
