package assembler.x86.operations

import assembler.x86.operands.{ImmediateValue, ValueSize}
import assembler.x86.operations.OperandInfo.OperandOrder._

sealed trait ImmediateBytes {
  self: X86Operation =>
  def immediateBytes: Seq[Byte]
  private[operations] def immediateInit(): Unit
}

trait NoImmediate extends ImmediateBytes {
  self: X86Operation =>
  override def immediateBytes: Seq[Byte] = Nil
  override final def immediateInit(): Unit = Unit
}

trait Immediate[Size<:ValueSize] extends ImmediateBytes {

  self: X86Operation =>
  def immediate: ImmediateValue with Size
  def immediateOrder: OperandOrder

  override final def immediateInit(): Unit =
    addOperand(OperandInfo.immediate(immediate, immediateOrder))

  override def immediateBytes: Seq[Byte] = immediate.value
}
