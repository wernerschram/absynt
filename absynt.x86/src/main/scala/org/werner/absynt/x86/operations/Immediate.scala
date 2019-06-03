package org.werner.absynt.x86.operations

import org.werner.absynt.x86.HasOperandSizePrefixRequirements
import org.werner.absynt.x86.operands.{ImmediateValue, ValueSize}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._

sealed trait ImmediateBytes {
  self: X86Operation =>
  def immediateBytes: Seq[Byte]
  protected override def allOperands: Set[OperandInfo[_]]
}

trait NoImmediate extends ImmediateBytes {
  self: X86Operation =>
  override def immediateBytes: Seq[Byte] = Nil
}

trait Immediate[Size<:ValueSize] extends ImmediateBytes {

  self: X86Operation with HasOperandSizePrefixRequirements =>

  def immediate: ImmediateValue with Size
  def immediateOrder: OperandOrder

  protected override abstract def allOperands: Set[OperandInfo[_]] =
    super.allOperands + OperandInfo.immediate(immediate, immediateOrder)

  override def immediateBytes: Seq[Byte] = immediate.value
}
